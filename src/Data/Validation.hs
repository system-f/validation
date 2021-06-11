{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

-- | A data type similar to @Data.Either@ that accumulates failures.
module Data.Validation
(
  -- * Data type
  Validation(..)
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
  -- choice of Either or Validation. This choice can then be made later by a
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
import Control.DeepSeq (NFData (rnf))
import Control.Lens (over, under)
import Control.Lens.Getter((^.))
import Control.Lens.Iso(Iso, iso, from
#if !MIN_VERSION_lens(4,20,0)
                       , Swapped(..))
#else
                       )
#endif
import Control.Lens.Prism(Prism, _Left, _Right)
import Control.Lens.Review(( # ))
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bifunctor.Swap(Swap(..))
import Data.Bitraversable(Bitraversable(bitraverse))
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
import GHC.Generics (Generic)
import Prelude(Show, Maybe(..))


-- | A @Validation@ is either a value of the type @err@ or @a@, similar to 'Either'. However,
-- the 'Applicative' instance for @Validation@ /accumulates/ errors using a 'Semigroup' on @err@.
-- In contrast, the @Applicative@ for @Either@ returns only the first error.
--
-- A consequence of this is that @Validation@ has no 'Data.Functor.Bind.Bind' or 'Control.Monad.Monad' instance. This is because
-- such an instance would violate the law that a Monad's 'Control.Monad.ap' must equal the
-- @Applicative@'s 'Control.Applicative.<*>'
--
-- An example of typical usage can be found <https://github.com/qfpl/validation/blob/master/examples/src/Email.hs here>.
--
data Validation err a =
  Failure err
  | Success a
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance Functor (Validation err) where
  fmap _ (Failure e) =
    Failure e
  fmap f (Success a) =
    Success (f a)
  {-# INLINE fmap #-}

instance Semigroup err => Apply (Validation err) where
  Failure e1 <.> b = Failure $ case b of
    Failure e2 -> e1 <> e2
    Success _ -> e1
  Success _  <.> Failure e2 =
    Failure e2
  Success f  <.> Success a  =
    Success (f a)
  {-# INLINE (<.>) #-}

instance Semigroup err => Applicative (Validation err) where
  pure =
    Success
  (<*>) =
    (<.>)

-- | For two errors, this instance reports only the last of them.
instance Alt (Validation err) where
  Failure _ <!> x =
    x
  Success a <!> _ =
    Success a
  {-# INLINE (<!>) #-}

instance Foldable (Validation err) where
  foldr f x (Success a) =
    f a x
  foldr _ x (Failure _) =
    x
  {-# INLINE foldr #-}

instance Traversable (Validation err) where
  traverse f (Success a) =
    Success <$> f a
  traverse _ (Failure e) =
    pure (Failure e)
  {-# INLINE traverse #-}

instance Bifunctor Validation where
  bimap f _ (Failure e) =
    Failure (f e)
  bimap _ g (Success a) =
    Success (g a)
  {-# INLINE bimap #-}


instance Bifoldable Validation where
  bifoldr _ g x (Success a) =
    g a x
  bifoldr f _ x (Failure e) =
    f e x
  {-# INLINE bifoldr #-}

instance Bitraversable Validation where
  bitraverse _ g (Success a) =
    Success <$> g a
  bitraverse f _ (Failure e) =
    Failure <$> f e
  {-# INLINE bitraverse #-}

appValidation ::
  (err -> err -> err)
  -> Validation err a
  -> Validation err a
  -> Validation err a
appValidation m (Failure e1) (Failure e2) =
  Failure (e1 `m` e2)
appValidation _ (Failure _) (Success a2) =
  Success a2
appValidation _ (Success a1) (Failure _) =
  Success a1
appValidation _ (Success a1) (Success _) =
  Success a1
{-# INLINE appValidation #-}

instance Semigroup e => Semigroup (Validation e a) where
  (<>) =
    appValidation (<>)
  {-# INLINE (<>) #-}

instance Monoid e => Monoid (Validation e a) where
  mappend =
    appValidation mappend
  {-# INLINE mappend #-}
  mempty =
    Failure mempty
  {-# INLINE mempty #-}

#if !MIN_VERSION_lens(4,20,0)
instance Swapped Validation where
  swapped = iso swap swap
  {-# INLINE swapped #-}
#endif

instance Swap Validation where
  swap v =
    case v of
      Failure e -> Success e
      Success a -> Failure a
  {-# INLINE swap #-}

instance (NFData e, NFData a) => NFData (Validation e a) where
  rnf v =
    case v of
      Failure e -> rnf e
      Success a -> rnf a

-- | 'validate's an @a@ producing an updated optional value, returning
-- @e@ in the empty case.
--
-- This can be thought of as having the less general type:
--
-- @
-- validate :: e -> (a -> Maybe b) -> a -> Validation e b
-- @
validate :: Validate v => e -> (a -> Maybe b) -> a -> v e b
validate e p a = case p a of
  Nothing -> _Failure # e
  Just b  -> _Success # b

-- | 'validationNel' is 'liftError' specialised to 'NonEmpty' lists, since
-- they are a common semigroup to use.
validationNel :: Either e a -> Validation (NonEmpty e) a
validationNel = liftError pure

-- | Converts from 'Either' to 'Validation'.
fromEither :: Either e a -> Validation e a
fromEither = liftError id

-- | 'liftError' is useful for converting an 'Either' to an 'Validation'
-- when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
liftError :: (b -> e) -> Either b a -> Validation e a
liftError f = either (Failure . f) Success

-- | 'validation' is the catamorphism for @Validation@.
validation :: (e -> c) -> (a -> c) -> Validation e a -> c
validation ec ac = \case
  Failure e -> ec e
  Success a -> ac a

-- | Converts from 'Validation' to 'Either'.
toEither :: Validation e a -> Either e a
toEither = validation Left Right

-- | @v 'orElse' a@ returns @a@ when @v@ is Failure, and the @a@ in @Success a@.
--
-- This can be thought of as having the less general type:
--
-- @
-- orElse :: Validation e a -> a -> a
-- @
orElse :: Validate v => v e a -> a -> a
orElse v a = case v ^. _Validation of
  Failure _ -> a
  Success x -> x

-- | Return the @a@ or run the given function over the @e@.
--
-- This can be thought of as having the less general type:
--
-- @
-- valueOr :: (e -> a) -> Validation e a -> a
-- @
valueOr :: Validate v => (e -> a) -> v e a -> a
valueOr ea v = case v ^. _Validation of
  Failure e -> ea e
  Success a -> a

-- | 'codiagonal' gets the value out of either side.
codiagonal :: Validation a a -> a
codiagonal = valueOr id

-- | 'ensure' ensures that a validation remains unchanged upon failure,
-- updating a successful validation with an optional value that could fail
-- with @e@ otherwise.
--
-- This can be thought of as having the less general type:
--
-- @
-- ensure :: e -> (a -> Maybe b) -> Validation e a -> Validation e b
-- @
ensure :: Validate v => e -> (a -> Maybe b) -> v e a -> v e b
ensure e p =
  over _Validation $ \case
    Failure x -> Failure x
    Success a -> validate e p a

-- | Run a function on anything with a Validate instance (usually Either)
-- as if it were a function on Validation
--
-- This can be thought of as having the type
--
-- @(Either e a -> Either e' a') -> Validation e a -> Validation e' a'@
validationed :: Validate v => (v e a -> v e' a') -> Validation e a -> Validation e' a'
validationed = under _Validation

-- | @bindValidation@ binds through a Validation, which is useful for
-- composing Validations sequentially. Note that despite having a bind
-- function of the correct type, Validation is not a monad.
-- The reason is, this bind does not accumulate errors, so it does not
-- agree with the Applicative instance.
--
-- There is nothing wrong with using this function, it just does not make a
-- valid @Monad@ instance.
bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation v f = case v of
  Failure e -> Failure e
  Success a -> f a

-- | The @Validate@ class carries around witnesses that the type @f@ is isomorphic
-- to Validation, and hence isomorphic to Either.
class Validate f where
  _Validation ::
    Iso (f e a) (f g b) (Validation e a) (Validation g b)

  _Either ::
    Iso (f e a) (f g b) (Either e a) (Either g b)
  _Either = _Validation . iso toEither fromEither
  {-# INLINE _Either #-}

instance Validate Validation where
  _Validation =
    id
  {-# INLINE _Validation #-}

instance Validate Either where
  _Validation =
    iso
      fromEither
      toEither
  {-# INLINE _Validation #-}
  _Either =
    id
  {-# INLINE _Either #-}

-- | This prism generalises 'Control.Lens.Prism._Left'. It targets the failure case of either 'Either' or 'Validation'.
_Failure ::
  Validate f =>
  Prism (f e1 a) (f e2 a) e1 e2
_Failure = _Either . _Left
{-# INLINE [1] _Failure #-}

-- | This prism generalises 'Control.Lens.Prism._Right'. It targets the success case of either 'Either' or 'Validation'.
_Success ::
  Validate f =>
  Prism (f e a) (f e b) a b
_Success = _Either . _Right
{-# INLINE _Success #-}

-- | 'revalidate' converts between any two instances of 'Validate'.
revalidate :: (Validate f, Validate g) => Iso (f e1 s) (f e2 t) (g e1 s) (g e2 t)
revalidate = _Validation . from _Validation
