{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Data types similar to @Data.Either@ that are explicit about failure and success.
module Data.Validation
(
  -- * Data types
  AccValidation(..)
, Validation(..)
, ValidationT(..)
, Validation'
  -- * Prisms
, _Failure
, _Success
  -- * Isomorphisms
, Validate(..)
) where

import Control.Applicative(Applicative((<*>), pure), liftA2, (<$>))
import Control.Lens.Getter((^.))
import Control.Lens.Iso(Swapped(..), Iso, iso)
import Control.Lens.Prism(Prism, prism)
import Control.Lens.Review(( # ))
import Control.Monad(Monad((>>=), return), liftM)
import Control.Monad.Trans.Class(MonadTrans, lift)
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Data(Data)
import Data.Either(Either(Left, Right))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldr))
import Data.Function((.), id, flip)
import Data.Functor(Functor(fmap))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)), liftF2)
import Data.Functor.Identity(Identity(Identity))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Data.Typeable(Typeable)
import Prelude(Show)

-- $setup
-- >>> import Prelude(Num(..))
-- >>> import Data.Eq(Eq(..))
-- >>> import Data.String(String)
-- >>> import Data.Int(Int)
-- >>> import Test.QuickCheck
-- >>> import Data.Either(either)
-- >>> instance (Arbitrary err, Arbitrary a) => Arbitrary (AccValidation err a) where arbitrary = fmap (either (_Failure #) (_Success #)) arbitrary
-- >>> instance (Arbitrary err, Arbitrary a) => Arbitrary (Validation err a) where arbitrary = fmap (either (_Failure #) (_Success #)) arbitrary
-- >>> instance (Applicative m, Arbitrary err, Arbitrary a) => Arbitrary (ValidationT err m a) where arbitrary = fmap (ValidationT . pure) arbitrary
-- >>> instance (Applicative m, Arbitrary err, Arbitrary a) => Arbitrary (ValidationB m err a) where arbitrary = fmap (ValidationB . pure) arbitrary

-- | A value of the type @err@ or @a@, however, the @Applicative@ instance
-- accumulates values. This is witnessed by the @Semigroup@ context on the instance.
-- /Note that there is no Monad such that ap = (<*>)./
--
-- >>> _Success # (+1) <*> _Success # 7 :: AccValidation String Int
-- AccSuccess 8
--
-- >>> _Failure # ["f1"] <*> _Success # 7 :: AccValidation [String] Int
-- AccFailure ["f1"]
--
-- >>> _Success # (+1) <*> _Failure # ["f2"] :: AccValidation [String] Int
-- AccFailure ["f2"]
--
-- >>> _Failure # ["f1"] <*> _Failure # ["f2"] :: AccValidation [String] Int
-- AccFailure ["f1","f2"]
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

instance Semigroup err => Alt (AccValidation err) where
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

-- |
--
-- prop> ((x <> y) <> z) == (x <> (y <> z :: AccValidation [String] Int))
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

-- |
--
-- prop> ((x `mappend` y) `mappend` z) == (x `mappend` (y `mappend` z :: AccValidation [String] Int))
--
-- prop> mempty `mappend` x == (x :: AccValidation [String] Int)
--
-- prop> x `mappend` mempty == (x :: AccValidation [String] Int)
instance Monoid e => Monoid (AccValidation e a) where
  mappend =
    appmAccValidation
  mempty =
    emptyAccValidation

-- | A value of the type @err@ or @a@ and isomorphic to @Data.Either@.
--
-- >>> _Success # (+1) <*> _Success # 7 :: Validation String Int
-- Success 8
--
-- >>> _Failure # ["f1"] <*> _Success # 7 :: Validation [String] Int
-- Failure ["f1"]
--
-- >>> _Success # (+1) <*> _Failure # ["f2"] :: Validation [String] Int
-- Failure ["f2"]
--
-- >>> _Failure # ["f1"] <*> _Failure # ["f2"] :: Validation [String] Int
-- Failure ["f1"]
data Validation err a =
  Failure err
  | Success a
  deriving (Eq, Ord, Show, Data, Typeable)

fmapValidation ::
  (a -> b)
  -> Validation err a
  -> Validation err b
fmapValidation _ (Failure e) =
  Failure e
fmapValidation f (Success a) =
  Success (f a)
{-# INLINE fmapValidation #-}

instance Functor (Validation err) where
  fmap =
    fmapValidation

apValidation ::
  Validation err (a -> b)
  -> Validation err a
  -> Validation err b
Failure e1 `apValidation` Failure _  =
  Failure e1
Failure e1 `apValidation` Success _  =
  Failure e1
Success _  `apValidation` Failure e2 =
  Failure e2
Success f  `apValidation` Success a  =
  Success (f a)
{-# INLINE apValidation #-}

instance Apply (Validation err) where
  (<.>) =
    apValidation

instance Applicative (Validation err) where
  pure =
    Success
  (<*>) =
    apValidation

altValidation ::
  Validation err a
  -> Validation err a
  -> Validation err a
Failure _ `altValidation` x =
  x
Success a `altValidation` _ =
  Success a
{-# INLINE altValidation #-}

instance Alt (Validation err) where
  (<!>) =
    altValidation

foldrValidation ::
  (a -> b -> b)
  -> b
  -> Validation err a
  -> b
foldrValidation f x (Success a) =
  f a x
foldrValidation _ x (Failure _) =
  x
{-# INLINE foldrValidation #-}

instance Foldable (Validation err) where
  foldr =
    foldrValidation

traverseValidation ::
  Applicative f =>
  (a -> f b)
  -> Validation err a
  -> f (Validation err b)
traverseValidation f (Success a) =
  Success <$> f a
traverseValidation _ (Failure e) =
  pure (Failure e)
{-# INLINE traverseValidation #-}

instance Traversable (Validation err) where
  traverse =
    traverseValidation

bimapValidation ::
  (err -> f)
  -> (a -> b)
  -> Validation err a
  -> Validation f b
bimapValidation f _ (Failure e) =
  Failure (f e)
bimapValidation _ g (Success a) =
  Success (g a)
{-# INLINE bimapValidation #-}

instance Bifunctor Validation where
  bimap =
    bimapValidation

bifoldrValidation ::
  (x -> a -> b)
  -> (y -> a -> b)
  -> a
  -> Validation x y
  -> b
bifoldrValidation _ g x (Success a) =
  g a x
bifoldrValidation f _ x (Failure e) =
  f e x
{-# INLINE bifoldrValidation #-}

instance Bifoldable Validation where
  bifoldr =
    bifoldrValidation

bitraverseValidation ::
  Functor f =>
  (x -> f err)
  -> (y -> f a)
  -> Validation x y
  -> f (Validation err a)
bitraverseValidation _ g (Success a) =
  Success <$> g a
bitraverseValidation f _ (Failure e) =
  Failure <$> f e
{-# INLINE bitraverseValidation #-}

instance Bitraversable Validation where
  bitraverse =
    bitraverseValidation

bindValidation ::
  Validation err a
  -> (a -> Validation err b)
  -> Validation err b
Failure e `bindValidation` _ =
  Failure e
Success a `bindValidation` f =
  f a
{-# INLINE bindValidation #-}

instance Bind (Validation err) where
  (>>-) =
    bindValidation

instance Monad (Validation err) where
  return =
    Success
  (>>=) =
    bindValidation

-- | The transformer version of @Validation@.
data ValidationT err m a =
  ValidationT {
    runValidationT :: m (Validation err a)
  }

type Validation' err a =
  ValidationT err Identity a

fmapValidationT ::
  Functor f =>
  (a -> b)
  -> ValidationT err f a
  -> ValidationT err f b
fmapValidationT f (ValidationT k) =
  ValidationT (fmap (fmap f) k)
{-# INLINE fmapValidationT #-}

instance Functor m => Functor (ValidationT err m) where
  fmap =
    fmapValidationT

apValidationT ::
  Apply f =>
  ValidationT err f (a -> b)
  -> ValidationT err f a
  -> ValidationT err f b
ValidationT f `apValidationT` ValidationT a =
    ValidationT (liftF2 (<.>) f a)
{-# INLINE apValidationT #-}

instance Apply m => Apply (ValidationT err m) where
  (<.>) =
    apValidationT

pureValidationT ::
  Applicative f =>
  a
  -> ValidationT err f a
pureValidationT =
  ValidationT . pure . pure
{-# INLINE pureValidationT #-}

aplValidationT ::
  Applicative f =>
  ValidationT err f (a -> b)
  -> ValidationT err f a
  -> ValidationT err f b
ValidationT f `aplValidationT` ValidationT a =
    ValidationT (liftA2 (<*>) f a)
{-# INLINE aplValidationT #-}

instance Applicative m => Applicative (ValidationT err m) where
  pure =
    pureValidationT
  (<*>) =
    aplValidationT

altValidationT ::
  (Functor m, Monad m) =>
  ValidationT err m a
  -> ValidationT err m a
  -> ValidationT err m a
ValidationT x `altValidationT` ValidationT y =
  ValidationT (x >>= \q -> case q of
    Failure _ -> y
    Success a -> return (Success a))
{-# INLINE altValidationT #-}

instance (Functor m, Monad m) => Alt (ValidationT err m) where
  (<!>) =
    altValidationT

foldrValidationT ::
  Foldable f =>
  (a -> b -> b)
  -> b
  -> ValidationT err f a
  -> b
foldrValidationT f z (ValidationT x) =
  foldr (flip (foldr f)) z x
{-# INLINE foldrValidationT #-}

instance Foldable m => Foldable (ValidationT err m) where
  foldr =
    foldrValidationT

traverseValidationT ::
  (Traversable g, Applicative f) =>
  (a -> f b)
  -> ValidationT err g a
  -> f (ValidationT err g b)
traverseValidationT f (ValidationT x) =
  ValidationT <$> traverse (traverse f) x
{-# INLINE traverseValidationT #-}

instance Traversable m => Traversable (ValidationT err m) where
  traverse =
    traverseValidationT

bindValidationT ::
  Monad f =>
  ValidationT err f a
  -> (a -> ValidationT err f b)
  -> ValidationT err f b
ValidationT v `bindValidationT` f =
  ValidationT (v >>= \w -> case w of
                             Failure e -> return (Failure e)
                             Success a -> runValidationT (f a))
{-# INLINE bindValidationT #-}

instance (Apply m, Monad m) => Bind (ValidationT err m) where
  (>>-) =
    bindValidationT

returnValidationT ::
  Monad f =>
  a
  -> ValidationT err f a
returnValidationT =
  ValidationT . return . pure
{-# INLINE returnValidationT #-}

instance Monad m => Monad (ValidationT err m) where
  return =
    returnValidationT
  (>>=) =
    bindValidationT

instance MonadTrans (ValidationT err) where
  lift = liftValidationT

liftValidationT ::
  Monad m =>
  m a
  -> ValidationT e m a
liftValidationT =
  ValidationT . liftM Success
{-# INLINE liftValidationT #-}

-- | The bifunctor version of ValidationT
data ValidationB m err a =
  ValidationB {
    runValidationB :: m (Validation err a)
  }

fmapValidationB ::
  Functor f =>
  (a -> b)
  -> ValidationB f err a
  -> ValidationB f err b
fmapValidationB f (ValidationB k) =
  ValidationB (fmap (fmap f) k)
{-# INLINE fmapValidationB #-}

instance Functor m => Functor (ValidationB m err) where
  fmap =
    fmapValidationB

apValidationB ::
  Apply f =>
  ValidationB f err (a -> b)
  -> ValidationB f err a
  -> ValidationB f err b
ValidationB f `apValidationB` ValidationB a =
    ValidationB (liftF2 (<.>) f a)
{-# INLINE apValidationB #-}

instance Apply m => Apply (ValidationB m err) where
  (<.>) =
    apValidationB

pureValidationB ::
  Applicative f =>
  a
  -> ValidationB f err a
pureValidationB =
  ValidationB . pure . pure
{-# INLINE pureValidationB #-}

aplValidationB ::
  Applicative f =>
  ValidationB f err (a -> b)
  -> ValidationB f err a
  -> ValidationB f err b
ValidationB f `aplValidationB` ValidationB a =
    ValidationB (liftA2 (<*>) f a)
{-# INLINE aplValidationB #-}

instance Applicative m => Applicative (ValidationB m err) where
  pure =
    pureValidationB
  (<*>) =
    aplValidationB

altValidationB ::
  (Functor m, Monad m) =>
  ValidationB m err a
  -> ValidationB m err a
  -> ValidationB m err a
ValidationB x `altValidationB` ValidationB y =
  ValidationB (x >>= \q -> case q of
    Failure _ -> y
    Success a -> return (Success a))
{-# INLINE altValidationB #-}

instance (Functor m, Monad m) => Alt (ValidationB m err) where
  (<!>) =
    altValidationB

foldrValidationB ::
  Foldable f =>
  (a -> b -> b)
  -> b
  -> ValidationB f err a
  -> b
foldrValidationB f z (ValidationB x) =
  foldr (flip (foldr f)) z x
{-# INLINE foldrValidationB #-}

instance Foldable m => Foldable (ValidationB m err) where
  foldr =
    foldrValidationB

traverseValidationB ::
  (Traversable g, Applicative f) =>
  (a -> f b)
  -> ValidationB g err a
  -> f (ValidationB g err b)
traverseValidationB f (ValidationB x) =
  ValidationB <$> traverse (traverse f) x
{-# INLINE traverseValidationB #-}

instance Traversable m => Traversable (ValidationB m err) where
  traverse =
    traverseValidationB

bimapValidationB ::
  Functor f =>
  (err -> frr)
  -> (a -> b)
  -> ValidationB f err a
  -> ValidationB f frr b
bimapValidationB f g (ValidationB x) =
  ValidationB (fmap (bimap f g) x)
{-# INLINE bimapValidationB #-}

instance Functor m => Bifunctor (ValidationB m) where
  bimap =
    bimapValidationB

bifoldrValidationB ::
  Foldable f =>
  (err -> b -> b)
  -> (a -> b -> b)
  -> b
  -> ValidationB f err a
  -> b
bifoldrValidationB f g z (ValidationB x) =
  foldr (flip (bifoldr f g)) z x
{-# INLINE bifoldrValidationB #-}

instance Foldable m => Bifoldable (ValidationB m) where
  bifoldr =
    bifoldrValidationB

bitraverseValidationB ::
  (Traversable g, Applicative f) =>
  (err -> f frr)
  -> (a -> f b)
  -> ValidationB g err a
  -> f (ValidationB g frr b)
bitraverseValidationB f g (ValidationB x) =
  ValidationB <$> traverse (bitraverse f g) x
{-# INLINE bitraverseValidationB #-}

instance Traversable m => Bitraversable (ValidationB m) where
  bitraverse =
    bitraverseValidationB

bindValidationB ::
  Monad f =>
  ValidationB f err a
  -> (a -> ValidationB f err b)
  -> ValidationB f err b
ValidationB v `bindValidationB` f =
  ValidationB (v >>= \w -> case w of
                             Failure e -> return (Failure e)
                             Success a -> runValidationB (f a))
{-# INLINE bindValidationB #-}
instance (Apply m, Monad m) => Bind (ValidationB m err) where
  (>>-) =
    bindValidationB

returnValidationB ::
  Monad f =>
  a
  -> ValidationB f err a
returnValidationB =
  ValidationB . return . pure
{-# INLINE returnValidationB #-}

instance Monad m => Monad (ValidationB m err) where
  return =
    returnValidationB
  (>>=) =
    bindValidationB

_ValidationV' ::
  Validate f =>
  Iso (f e a) (f g b) (Validation' e a) (Validation' g b)
_ValidationV' =
  iso
    (\x -> ValidationT (Identity (x ^. _Validation)))
    (\(ValidationT (Identity x)) -> _Validation # x)
{-# INLINE _ValidationV' #-}

_ValidationTx ::
  Iso (ValidationT e m a) (ValidationT e' m' a') (ValidationB m e a) (ValidationB m' e' a')
_ValidationTx =
  iso
    (\(ValidationT x) -> ValidationB x)
    (\(ValidationB x) -> ValidationT x)

_AccValidationV ::
  Validate f =>
  Iso (f e a) (f g b) (AccValidation e a) (AccValidation g b)
_AccValidationV =
  iso
    (\x -> case x ^. _Validation of
             Failure e -> AccFailure e
             Success a -> AccSuccess a)
    (\x -> _Validation # case x of
                           AccFailure e -> Failure e
                           AccSuccess a -> Success a)
{-# INLINE _AccValidationV #-}

_EitherV ::
  Validate f =>
  Iso (f e a) (f g b) (Either e a) (Either g b)
_EitherV =
  iso
    (\x -> case x ^. _Validation of
             Failure e -> Left e
             Success a -> Right a)
    (\x -> _Validation # case x of
                           Left e -> Failure e
                           Right a -> Success a)
{-# INLINE _EitherV #-}

class Validate f where
  _Validation ::
    Iso (f e a) (f g b) (Validation e a) (Validation g b)

  _Validation' ::
    Iso (f e a) (f g b) (Validation' e a) (Validation' g b)
  _Validation' =
    _ValidationV'

  _AccValidation ::
    Iso (f e a) (f g b) (AccValidation e a) (AccValidation g b)
  _AccValidation =
    _AccValidationV

  _Either ::
    Iso (f e a) (f g b) (Either e a) (Either g b)
  _Either =
    _EitherV

instance Validate Validation where
  _Validation =
    id

_AccValidationValidationIso ::
  Iso (AccValidation e a) (AccValidation g b) (Validation e a) (Validation g b)
_AccValidationValidationIso =
  iso
    (\x -> case x of
             AccFailure e -> Failure e
             AccSuccess a -> Success a)
    (\x -> case x of
             Failure e -> AccFailure e
             Success a -> AccSuccess a)
{-# INLINE _AccValidationValidationIso #-}

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
  _Validation =
    _AccValidationValidationIso
  _AccValidation =
    id
  _Either =
    _AccValidationEitherIso

_EitherValidationIso ::
  Iso (Either e a) (Either g b) (Validation e a) (Validation g b)
_EitherValidationIso =
  iso
    (\x -> case x of
             Left e -> Failure e
             Right a -> Success a)
    (\x -> case x of
             Failure e -> Left e
             Success a -> Right a)
{-# INLINE _EitherValidationIso #-}

_EitherAccValidationIso ::
  Iso (Either e a) (Either g b) (AccValidation e a) (AccValidation g b)
_EitherAccValidationIso =
  iso
    (\x -> case x of
             Left e -> AccFailure e
             Right a -> AccSuccess a)
    (\x -> case x of
             AccFailure e -> Left e
             AccSuccess a -> Right a)
{-# INLINE _EitherAccValidationIso #-}

instance Validate Either where
  _Validation =
    _EitherValidationIso
  _AccValidation =
    _EitherAccValidationIso
  _Either =
    id

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

swappedValidation ::
  Iso (Validation e a) (Validation f b) (Validation a e) (Validation b f)
swappedValidation =
  iso
    (\v -> case v of
             Failure e -> Success e
             Success a -> Failure a)
    (\v -> case v of
             Failure a -> Success a
             Success e -> Failure e)
{-# INLINE swappedValidation #-}

swappedValidationB ::
  Functor k =>
  Iso (ValidationB k e a) (ValidationB k f b) (ValidationB k a e) (ValidationB k b f)
swappedValidationB =
  iso
    (\(ValidationB x) -> ValidationB (fmap (swapped # ) x))
    (\(ValidationB x) -> ValidationB (fmap (swapped # ) x))
{-# INLINE swappedValidationB #-}

instance Swapped AccValidation where
  swapped =
    swappedAccValidation

instance Swapped Validation where
  swapped =
    swappedValidation

instance Functor f => Swapped (ValidationB f) where
  swapped =
    swappedValidationB
