{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

-- | A data type similar to @Data.Either@ that accumulates failures.
module Data.Validation
(
  -- * Data type
  AccValidation(..)
  -- * Constructing validations
, validate
, validationNel
, fromEither
, liftError
  -- * Functions on validations
, validation
, toEither
, orElse
, valueOr
, ensure
, codiagonal
, validationed
, bindValidation
  -- * Prisms
  -- | These prisms are useful for writing code which is polymorphic in its
  -- choice of Either or AccValidation. This choice can then be made later by a
  -- user, depending on their needs.
  --
  -- An example of this style of usage can be found
  -- <https://github.com/qfpl/validation/blob/master/examples/src/PolymorphicEmail.hs here>
, _Failure
, _Success
  -- * Isomorphisms
, Validate(..)
, revalidate
) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import Control.Lens (over, under)
import Control.Lens.Getter((^.))
import Control.Lens.Iso(Swapped(..), Iso, iso, from)
import Control.Lens.Prism(Prism, prism)
import Control.Lens.Review(( # ))
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Bool (Bool)
import Data.Data(Data)
import Data.Either(Either(Left, Right), either)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldr))
import Data.Function((.), ($), id)
import Data.Functor(Functor(fmap))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Data.Typeable(Typeable)
import Prelude(Show)


-- | An @AccValidation@ is either a value of the type @err@ or @a@, similar to 'Either'. However,
-- the 'Applicative' instance for @AccValidation@ /accumulates/ errors using a 'Semigroup' on @err@.
-- In contrast, the @Applicative@ for @Either@ returns only the first error.
--
-- A consequence of this is that @AccValidation@ has no 'Data.Functor.Bind.Bind' or 'Control.Monad.Monad' instance. This is because
-- such an instance would violate the law that a Monad's 'Control.Monad.ap' must equal the
-- @Applicative@'s 'Control.Applicative.<*>'
--
-- An example of typical usage can be found <https://github.com/qfpl/validation/blob/master/examples/src/Email.hs here>.
--
data AccValidation err a =
  AccFailure err
  | AccSuccess a
  deriving (Eq, Ord, Show, Data, Typeable)

instance Functor (AccValidation err) where
  fmap _ (AccFailure e) =
    AccFailure e
  fmap f (AccSuccess a) =
    AccSuccess (f a)
  {-# INLINE fmap #-}

instance Semigroup err => Apply (AccValidation err) where
  AccFailure e1 <.> AccFailure e2 =
    AccFailure (e1 <> e2)
  AccFailure e1 <.> AccSuccess _  =
    AccFailure e1
  AccSuccess _  <.> AccFailure e2 =
    AccFailure e2
  AccSuccess f  <.> AccSuccess a  =
    AccSuccess (f a)
  {-# INLINE (<.>) #-}

instance Semigroup err => Applicative (AccValidation err) where
  pure =
    AccSuccess
  (<*>) =
    (<.>)

instance Alt (AccValidation err) where
  AccFailure _ <!> x =
    x
  AccSuccess a <!> _ =
    AccSuccess a
  {-# INLINE (<!>) #-}

instance Foldable (AccValidation err) where
  foldr f x (AccSuccess a) =
    f a x
  foldr _ x (AccFailure _) =
    x
  {-# INLINE foldr #-}

instance Traversable (AccValidation err) where
  traverse f (AccSuccess a) =
    AccSuccess <$> f a
  traverse _ (AccFailure e) =
    pure (AccFailure e)
  {-# INLINE traverse #-}

instance Bifunctor AccValidation where
  bimap f _ (AccFailure e) =
    AccFailure (f e)
  bimap _ g (AccSuccess a) =
    AccSuccess (g a)
  {-# INLINE bimap #-}


instance Bifoldable AccValidation where
  bifoldr _ g x (AccSuccess a) =
    g a x
  bifoldr f _ x (AccFailure e) =
    f e x
  {-# INLINE bifoldr #-}

instance Bitraversable AccValidation where
  bitraverse _ g (AccSuccess a) =
    AccSuccess <$> g a
  bitraverse f _ (AccFailure e) =
    AccFailure <$> f e
  {-# INLINE bitraverse #-}

appAccValidation ::
  (err -> err -> err)
  -> AccValidation err a
  -> AccValidation err a
  -> AccValidation err a
appAccValidation m (AccFailure e1) (AccFailure e2) =
  AccFailure (e1 `m` e2)
appAccValidation _ (AccFailure _) (AccSuccess a2) =
  AccSuccess a2
appAccValidation _ (AccSuccess a1) (AccFailure _) =
  AccSuccess a1
appAccValidation _ (AccSuccess a1) (AccSuccess _) =
  AccSuccess a1
{-# INLINE appAccValidation #-}

instance Semigroup e => Semigroup (AccValidation e a) where
  (<>) =
    appAccValidation (<>)
  {-# INLINE (<>) #-}

instance Monoid e => Monoid (AccValidation e a) where
  mappend =
    appAccValidation mappend
  {-# INLINE mappend #-}
  mempty =
    AccFailure mempty
  {-# INLINE mempty #-}

instance Swapped AccValidation where
  swapped =
    iso
      (\v -> case v of
        AccFailure e -> AccSuccess e
        AccSuccess a -> AccFailure a)
      (\v -> case v of
        AccFailure a -> AccSuccess a
        AccSuccess e -> AccFailure e)
  {-# INLINE swapped #-}

-- | 'validate's the @a@ with the given predicate, returning @e@ if the predicate does not hold.
--
-- This can be thought of as having the less general type:
--
-- @
-- validate :: e -> (a -> Bool) -> a -> AccValidation e a
-- @
validate :: Validate v => e -> (a -> Bool) -> a -> v e a
validate e p a =
  if p a then _Success # a else _Failure # e

-- | 'validationNel' is 'liftError' specialised to 'NonEmpty' lists, since
-- they are a common semigroup to use.
validationNel :: Either e a -> AccValidation (NonEmpty e) a
validationNel = liftError pure

-- | Converts from 'Either' to 'AccValidation'.
fromEither :: Either e a -> AccValidation e a
fromEither = liftError id

-- | 'liftError' is useful for converting an 'Either' to an 'AccValidation'
-- when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
liftError :: (b -> e) -> Either b a -> AccValidation e a
liftError f = either (AccFailure . f) AccSuccess

-- | 'validation' is the catamorphism for @AccValidation@.
validation :: (e -> c) -> (a -> c) -> AccValidation e a -> c
validation ec ac v = case v of
  AccFailure e -> ec e
  AccSuccess a -> ac a

-- | Converts from 'AccValidation' to 'Either'.
toEither :: AccValidation e a -> Either e a
toEither = validation Left Right

-- | @v 'orElse' a@ returns @a@ when @v@ is AccFailure, and the @a@ in @AccSuccess a@.
--
-- This can be thought of as having the less general type:
--
-- @
-- orElse :: AccValidation e a -> a -> a
-- @
orElse :: Validate v => v e a -> a -> a
orElse v a = case v ^. _AccValidation of
  AccFailure _ -> a
  AccSuccess x -> x

-- | Return the @a@ or run the given function over the @e@.
--
-- This can be thought of as having the less general type:
--
-- @
-- valueOr :: (e -> a) -> AccValidation e a -> a
-- @
valueOr :: Validate v => (e -> a) -> v e a -> a
valueOr ea v = case v ^. _AccValidation of
  AccFailure e -> ea e
  AccSuccess a -> a

-- | 'codiagonal' gets the value out of either side.
codiagonal :: AccValidation a a -> a
codiagonal = valueOr id

-- | 'ensure' leaves the validation unchanged when the predicate holds, or
-- fails with @e@ otherwise.
--
-- This can be thought of as having the less general type:
--
-- @
-- ensure :: e -> (a -> Bool) -> AccValidation e a -> AccValidation e a
-- @
ensure :: Validate v => e -> (a -> Bool) -> v e a -> v e a
ensure e p =
  over _AccValidation $ \v -> case v of
    AccFailure x -> AccFailure x
    AccSuccess a -> validate e p a

-- | Run a function on anything with a Validate instance (usually Either)
-- as if it were a function on AccValidation
--
-- This can be thought of as having the type
--
-- @(Either e a -> Either e' a') -> AccValidation e a -> AccValidation e' a'@
validationed :: Validate v => (v e a -> v e' a') -> AccValidation e a -> AccValidation e' a'
validationed f = under _AccValidation f

-- | @bindValidation@ binds through an AccValidation, which is useful for
-- composing AccValidations sequentially. Note that despite having a bind
-- function of the correct type, AccValidation is not a monad.
-- The reason is, this bind does not accumulate errors, so it does not
-- agree with the Applicative instance.
--
-- There is nothing wrong with using this function, it just does not make a
-- valid @Monad@ instance.
bindValidation :: AccValidation e a -> (a -> AccValidation e b) -> AccValidation e b
bindValidation v f = case v of
  AccFailure e -> AccFailure e
  AccSuccess a -> f a

-- | The @Validate@ class carries around witnesses that the type @f@ is isomorphic
-- to AccValidation, and hence isomorphic to Either.
class Validate f where
  _AccValidation ::
    Iso (f e a) (f g b) (AccValidation e a) (AccValidation g b)

  _Either ::
    Iso (f e a) (f g b) (Either e a) (Either g b)
  _Either =
    iso
      (\x -> case x ^. _AccValidation of
        AccFailure e -> Left e
        AccSuccess a -> Right a)
      (\x -> _AccValidation # case x of
        Left e -> AccFailure e
        Right a -> AccSuccess a)
  {-# INLINE _Either #-}

instance Validate AccValidation where
  _AccValidation =
    id
  {-# INLINE _AccValidation #-}
  _Either =
    iso
      (\x -> case x of
        AccFailure e -> Left e
        AccSuccess a -> Right a)
      (\x -> case x of
        Left e -> AccFailure e
        Right a -> AccSuccess a)
  {-# INLINE _Either #-}

instance Validate Either where
  _AccValidation =
    iso
      fromEither
      toEither
  {-# INLINE _AccValidation #-}
  _Either =
    id
  {-# INLINE _Either #-}

-- | This prism generalises 'Control.Lens.Prism._Left'. It targets the failure case of either 'Either' or 'AccValidation'.
_Failure ::
  Validate f =>
  Prism (f e1 a) (f e2 a) e1 e2
_Failure =
  prism
    (\x -> _Either # Left x)
    (\x -> case x ^. _Either of
             Left e -> Right e
             Right a -> Left (_Either # Right a))
{-# INLINE _Failure #-}

-- | This prism generalises 'Control.Lens.Prism._Right'. It targets the success case of either 'Either' or 'AccValidation'.
_Success ::
  Validate f =>
  Prism (f e a) (f e b) a b
_Success =
  prism
    (\x -> _Either # Right x)
    (\x -> case x ^. _Either of
             Left e -> Left (_Either # Left e)
             Right a -> Right a)
{-# INLINE _Success #-}

-- | 'revalidate' converts between any two instances of 'Validate'.
revalidate :: (Validate f, Validate g) => Iso (f e1 s) (f e2 t) (g e1 s) (g e2 t)
revalidate = _AccValidation . from _AccValidation

