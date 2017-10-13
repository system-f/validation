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
, diagonal
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
) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import Control.Lens.Getter((^.))
import Control.Lens.Iso(Swapped(..), Iso, iso)
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
import Data.Function(id, (.))
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

fmapAccValidation ::
  (a -> b)
  -> AccValidation err a
  -> AccValidation err b
fmapAccValidation _ (AccFailure e) =
  AccFailure e
fmapAccValidation f (AccSuccess a) =
  AccSuccess (f a)
{-# INLINE fmapAccValidation #-}

instance Functor (AccValidation err) where
  fmap =
    fmapAccValidation

apAccValidation ::
  Semigroup err =>
  AccValidation err (a -> b)
  -> AccValidation err a
  -> AccValidation err b
AccFailure e1 `apAccValidation` AccFailure e2 =
  AccFailure (e1 <> e2)
AccFailure e1 `apAccValidation` AccSuccess _  =
  AccFailure e1
AccSuccess _  `apAccValidation` AccFailure e2 =
  AccFailure e2
AccSuccess f  `apAccValidation` AccSuccess a  =
  AccSuccess (f a)
{-# INLINE apAccValidation #-}

instance Semigroup err => Apply (AccValidation err) where
  (<.>) =
    apAccValidation

instance Semigroup err => Applicative (AccValidation err) where
  pure =
    AccSuccess
  (<*>) =
    (<.>)

altAccValidation ::
  AccValidation err a
  -> AccValidation err a
  -> AccValidation err a
AccFailure _ `altAccValidation` x =
  x
AccSuccess a `altAccValidation` _ =
  AccSuccess a
{-# INLINE altAccValidation #-}

instance Alt (AccValidation err) where
  (<!>) =
    altAccValidation

foldrAccValidation ::
  (a -> b -> b)
  -> b
  -> AccValidation err a -> b
foldrAccValidation f x (AccSuccess a) =
  f a x
foldrAccValidation _ x (AccFailure _) =
  x
{-# INLINE foldrAccValidation #-}

instance Foldable (AccValidation err) where
  foldr =
    foldrAccValidation

traverseAccValidation ::
  Applicative f =>
  (a -> f b)
  -> AccValidation err a
  -> f (AccValidation err b)
traverseAccValidation f (AccSuccess a) =
    AccSuccess <$> f a
traverseAccValidation _ (AccFailure e) =
  pure (AccFailure e)
{-# INLINE traverseAccValidation #-}

instance Traversable (AccValidation err) where
  traverse =
    traverseAccValidation

bimapAccValidation ::
  (err -> f)
  -> (a -> b)
  -> AccValidation err a
  -> AccValidation f b
bimapAccValidation f _ (AccFailure e) =
  AccFailure (f e)
bimapAccValidation _ g (AccSuccess a) =
  AccSuccess (g a)
{-# INLINE bimapAccValidation #-}

instance Bifunctor AccValidation where
  bimap =
    bimapAccValidation

bifoldrAccValidation ::
  (x -> a -> b)
  -> (y -> a -> b)
  -> a
  -> AccValidation x y
  -> b
bifoldrAccValidation _ g x (AccSuccess a) =
  g a x
bifoldrAccValidation f _ x (AccFailure e) =
  f e x
{-# INLINE bifoldrAccValidation #-}

instance Bifoldable AccValidation where
  bifoldr =
    bifoldrAccValidation

bitraverseAccValidation ::
  Functor f =>
  (x -> f err)
  -> (y -> f a)
  -> AccValidation x y
  -> f (AccValidation err a)
bitraverseAccValidation _ g (AccSuccess a) =
  AccSuccess <$> g a
bitraverseAccValidation f _ (AccFailure e) =
  AccFailure <$> f e
{-# INLINE bitraverseAccValidation #-}

instance Bitraversable AccValidation where
  bitraverse =
    bitraverseAccValidation

appsAccValidation ::
  Semigroup err =>
  AccValidation err a
  -> AccValidation err a
  -> AccValidation err a
AccFailure e1 `appsAccValidation` AccFailure e2 =
  AccFailure (e1 <> e2)
AccFailure _ `appsAccValidation` AccSuccess a2  =
  AccSuccess a2
AccSuccess a1  `appsAccValidation` AccFailure _ =
  AccSuccess a1
AccSuccess a1 `appsAccValidation` AccSuccess _ =
  AccSuccess a1
{-# INLINE appsAccValidation #-}

instance Semigroup e => Semigroup (AccValidation e a) where
  (<>) =
    appsAccValidation

appmAccValidation ::
  Monoid err =>
  AccValidation err a
  -> AccValidation err a
  -> AccValidation err a
AccFailure e1 `appmAccValidation` AccFailure e2 =
  AccFailure (e1 `mappend` e2)
AccFailure _ `appmAccValidation` AccSuccess a2  =
  AccSuccess a2
AccSuccess a1  `appmAccValidation` AccFailure _ =
  AccSuccess a1
AccSuccess a1 `appmAccValidation` AccSuccess _ =
  AccSuccess a1
{-# INLINE appmAccValidation #-}

emptyAccValidation ::
  Monoid err =>
  AccValidation err a
emptyAccValidation =
  AccFailure mempty
{-# INLINE emptyAccValidation #-}

instance Monoid e => Monoid (AccValidation e a) where
  mappend =
    appmAccValidation
  mempty =
    emptyAccValidation

_EitherV ::
  Validate f =>
  Iso (f e a) (f g b) (Either e a) (Either g b)
_EitherV =
  iso
    (\x -> case x ^. _AccValidation of
             AccFailure e -> Left e
             AccSuccess a -> Right a)
    (\x -> _AccValidation # case x of
                           Left e -> AccFailure e
                           Right a -> AccSuccess a)
{-# INLINE _EitherV #-}

swappedAccValidation ::
  Iso (AccValidation e a) (AccValidation f b) (AccValidation a e) (AccValidation b f)
swappedAccValidation =
  iso
    (\v -> case v of
             AccFailure e -> AccSuccess e
             AccSuccess a -> AccFailure a)
    (\v -> case v of
             AccFailure a -> AccSuccess a
             AccSuccess e -> AccFailure e)
{-# INLINE swappedAccValidation #-}

instance Swapped AccValidation where
  swapped =
    swappedAccValidation

-- | 'validate's the @a@ with the given predicate, returning @e@ if the predicate does not hold.
validate :: e -> (a -> Bool) -> a -> AccValidation e a
validate e p a =
  if p a then AccSuccess a else AccFailure e

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
orElse :: AccValidation e a -> a -> a
orElse v a = case v of
  AccFailure _ -> a
  AccSuccess x -> x

-- | Return the @a@ or run the given function over the @e@.
valueOr :: (e -> a) -> AccValidation e a -> a
valueOr ea v = case v of
  AccFailure e -> ea e
  AccSuccess a -> a

-- | 'diagonal' gets the value out of either side.
diagonal :: AccValidation a a -> a
diagonal = valueOr id

-- | 'ensure' leaves the validation unchanged when the predicate holds, or
-- fails with @e@ otherwise.
ensure :: e -> (a -> Bool) -> AccValidation e a -> AccValidation e a
ensure e p v = case v of
  AccFailure x -> AccFailure x
  AccSuccess a -> validate e p a

-- | The @Validate@ class carries around witnesses that the type @f@ is isomorphic
-- to AccValidation, and hence isomorphic to Either.
--
-- Its main use is to make '_Success' and '_Failure' work.
class Validate f where
  _AccValidation ::
    Iso (f e a) (f g b) (AccValidation e a) (AccValidation g b)

  _Either ::
    Iso (f e a) (f g b) (Either e a) (Either g b)
  _Either =
    _EitherV

_AccValidationEitherIso ::
  Iso (AccValidation e a) (AccValidation g b) (Either e a) (Either g b)
_AccValidationEitherIso =
  iso
    (\x -> case x of
             AccFailure e -> Left e
             AccSuccess a -> Right a)
    (\x -> case x of
             Left e -> AccFailure e
             Right a -> AccSuccess a)
{-# INLINE _AccValidationEitherIso #-}

instance Validate AccValidation where
  _AccValidation =
    id
  _Either =
    _AccValidationEitherIso

_EitherAccValidationIso ::
  Iso (Either e a) (Either g b) (AccValidation e a) (AccValidation g b)
_EitherAccValidationIso =
  iso
    fromEither
    toEither
{-# INLINE _EitherAccValidationIso #-}

instance Validate Either where
  _AccValidation =
    _EitherAccValidationIso
  _Either =
    id

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

