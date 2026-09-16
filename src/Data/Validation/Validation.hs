{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- \$setup
-- >>> import Prelude hiding (either, id, (.))
-- >>> import Control.Lens((^?), (#), review, view, from, set)
-- >>> import Data.Functor.Alt(Alt((<!>)))
-- >>> import Data.Functor.Apply(Apply((<.>)))
-- >>> import Control.DeepSeq(rnf)
-- >>> import Control.Category(id, (.))
-- >>> import Control.Selective(Selective(select))
-- >>> import Data.Bifunctor(Bifunctor(bimap))
-- >>> import Data.Bifoldable(Bifoldable(bifoldr))
-- >>> import Data.Bitraversable(Bitraversable(bitraverse))
-- >>> import Data.Bifunctor.Swap(Swap(swap))
-- >>> :set -XNoMonomorphismRestriction -w

-- | A data type similar to @Data.Either@ that accumulates failures.
module Data.Validation.Validation (
  -- * Data type
  Validation (..),

  -- * Catamorphism
  foldValidation,

  -- * Optics

  -- ** Classy lenses
  GetValidation (..),
  HasValidation (..),

  -- ** Classy prisms
  ReviewValidation (..),
  AsValidation (..),

  -- ** Prisms
  __Failure,
  __Success,

  -- ** Isomorphisms
  Data.Validation.Validation.either,
  codiagonal,
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Category (Category (..))
import Control.DeepSeq (NFData (rnf))
import Control.Lens (Getter, Lens', Prism, Prism', Review, from, prism, unto)
import Control.Lens.Iso (Iso, iso)
import Control.Selective (Selective (..))
import Data.Bifoldable (Bifoldable (bifoldr))
import Data.Bifoldable1 (Bifoldable1 (bifoldMap1))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bifunctor.Assoc (Assoc (assoc, unassoc))
import Data.Bifunctor.Swap (Swap (..))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Bool (bool)
import Data.Data (Data)
import qualified Data.Either as Either
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2), Ord1 (liftCompare), Ord2 (liftCompare2), Show1 (liftShowsPrec), Show2 (liftShowsPrec2), showsUnaryWith)
import Data.Functor.Extend (Extend (extended))
import Data.Functor.Plus (Plus (zero))
import Data.Semigroup.Traversable.Class (Bitraversable1 (bitraverse1))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Prelude hiding (either, id, (.))

{- | A @Validation@ is either a value of the type @err@ or @a@, similar to 'Either'. However,
the 'Applicative' instance for @Validation@ /accumulates/ errors using a 'Semigroup' on @err@.
In contrast, the @Applicative@ for @Either@ returns only the first error.

A consequence of this is that @Validation@ has no 'Data.Functor.Bind.Bind' or 'Control.Monad.Monad' instance. This is because
such an instance would violate the law that a Monad's 'Control.Monad.ap' must equal the
@Applicative@'s 'Control.Applicative.<*>'

See the <https://github.com/system-f/validation README> for usage examples.
-}
data Validation err a
  = Failure err
  | Success a
  deriving (Data, Eq, Generic, Generic1, Ord, Show, Typeable)

instance Eq2 Validation where
  liftEq2 f _ (Failure a) (Failure b) = f a b
  liftEq2 _ g (Success a) (Success b) = g a b
  liftEq2 _ _ _ _ = False
  {-# INLINE liftEq2 #-}

instance (Eq err) => Eq1 (Validation err) where
  liftEq = liftEq2 (==)
  {-# INLINE liftEq #-}

instance Ord2 Validation where
  liftCompare2 f _ (Failure a) (Failure b) = f a b
  liftCompare2 _ _ (Failure _) (Success _) = LT
  liftCompare2 _ _ (Success _) (Failure _) = GT
  liftCompare2 _ g (Success a) (Success b) = g a b
  {-# INLINE liftCompare2 #-}

instance (Ord err) => Ord1 (Validation err) where
  liftCompare = liftCompare2 compare
  {-# INLINE liftCompare #-}

instance Show2 Validation where
  liftShowsPrec2 sp1 _ _ _ d (Failure a) = showsUnaryWith sp1 "Failure" d a
  liftShowsPrec2 _ _ sp2 _ d (Success a) = showsUnaryWith sp2 "Success" d a
  {-# INLINE liftShowsPrec2 #-}

instance (Show err) => Show1 (Validation err) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
  {-# INLINE liftShowsPrec #-}

{- |
>>> fmap (+1) (Success 2 :: Validation String Int)
Success 3

>>> fmap (+1) (Failure "err" :: Validation String Int)
Failure "err"
-}
instance Functor (Validation err) where
  fmap _ (Failure e) =
    Failure e
  fmap f (Success a) =
    Success (f a)
  {-# INLINE fmap #-}

{- | Accumulates errors on the left using 'Semigroup'.

>>> import Data.Functor.Apply(Apply((<.>)))
>>> Success (+1) <.> Success 2 :: Validation [String] Int
Success 3

>>> Failure ["e1"] <.> Success 2 :: Validation [String] Int
Failure ["e1"]

>>> Success (+1) <.> Failure ["e2"] :: Validation [String] Int
Failure ["e2"]

>>> Failure ["e1"] <.> Failure ["e2"] :: Validation [String] Int
Failure ["e1","e2"]
-}
instance (Semigroup err) => Apply (Validation err) where
  Failure e1 <.> b = Failure $ case b of
    Failure e2 -> e1 <> e2
    Success _ -> e1
  Success _ <.> Failure e2 =
    Failure e2
  Success f <.> Success a =
    Success (f a)
  {-# INLINE (<.>) #-}

{- | Delegates to the 'Apply' instance, accumulating errors with '<>'.

>>> pure (+1) <*> pure 2 :: Validation [String] Int
Success 3

>>> Failure ["e1"] <*> Failure ["e2"] :: Validation [String] Int
Failure ["e1","e2"]
-}
instance (Semigroup err) => Applicative (Validation err) where
  pure =
    Success
  {-# INLINE pure #-}
  (<*>) =
    (<.>)
  {-# INLINE (<*>) #-}

{- | Tries the left, then the right, accumulating errors on two failures.

>>> import Data.Functor.Alt(Alt((<!>)))
>>> Success 1 <!> Success 2 :: Validation [String] Int
Success 1

>>> Failure ["e1"] <!> Success 2 :: Validation [String] Int
Success 2

>>> Success 1 <!> Failure ["e2"] :: Validation [String] Int
Success 1

>>> Failure ["e1"] <!> Failure ["e2"] :: Validation [String] Int
Failure ["e1","e2"]
-}
instance (Semigroup err) => Alt (Validation err) where
  Failure e1 <!> Failure e2 =
    Failure (e1 <> e2)
  Failure _ <!> Success a =
    Success a
  Success a <!> _ =
    Success a
  {-# INLINE (<!>) #-}

instance (Monoid err) => Plus (Validation err) where
  zero = Failure mempty
  {-# INLINE zero #-}

instance (Monoid err) => Alternative (Validation err) where
  empty = zero
  {-# INLINE empty #-}
  (<|>) = (<!>)
  {-# INLINE (<|>) #-}

{- | Skips the second effect on 'Failure'.

>>> import Control.Selective(Selective(select))
>>> select (Success (Right 1)) (Success (+1)) :: Validation [String] Int
Success 1

>>> select (Success (Left 1)) (Success (+1)) :: Validation [String] Int
Success 2

>>> select (Failure ["e1"]) (Success (+1)) :: Validation [String] Int
Failure ["e1"]

>>> select (Failure ["e1"]) (Failure ["e2"]) :: Validation [String] Int
Failure ["e1"]
-}
instance (Semigroup err) => Selective (Validation err) where
  select (Failure e) _ = Failure e
  select (Success x) f = Either.either (\a -> ($ a) <$> f) Success x
  {-# INLINE select #-}

{- |
>>> foldr (:) [] (Success 1 :: Validation String Int)
[1]

>>> foldr (:) [] (Failure "err" :: Validation String Int)
[]
-}
instance Foldable (Validation err) where
  foldr f x (Success a) =
    f a x
  foldr _ x (Failure _) =
    x
  {-# INLINE foldr #-}

{- |
>>> traverse (\x -> [x, x+1]) (Success 1 :: Validation String Int)
[Success 1,Success 2]

>>> traverse (\x -> [x, x+1]) (Failure "err" :: Validation String Int)
[Failure "err"]
-}
instance Traversable (Validation err) where
  traverse f (Success a) =
    Success <$> f a
  traverse _ (Failure e) =
    pure (Failure e)
  {-# INLINE traverse #-}

{- |
>>> import Data.Bifunctor(Bifunctor(bimap))
>>> bimap show (+1) (Failure 1 :: Validation Int Int)
Failure "1"

>>> bimap show (+1) (Success 1 :: Validation Int Int)
Success 2
-}
instance Bifunctor Validation where
  bimap f _ (Failure e) =
    Failure (f e)
  bimap _ g (Success a) =
    Success (g a)
  {-# INLINE bimap #-}

{- |
>>> import Data.Bifoldable(Bifoldable(bifoldr))
>>> bifoldr (\e r -> show e ++ r) (\a r -> show a ++ r) "" (Failure 1 :: Validation Int Int)
"1"

>>> bifoldr (\e r -> show e ++ r) (\a r -> show a ++ r) "" (Success 2 :: Validation Int Int)
"2"
-}
instance Bifoldable Validation where
  bifoldr _ g x (Success a) =
    g a x
  bifoldr f _ x (Failure e) =
    f e x
  {-# INLINE bifoldr #-}

instance Bifoldable1 Validation where
  bifoldMap1 f _ (Failure e) = f e
  bifoldMap1 _ g (Success a) = g a
  {-# INLINE bifoldMap1 #-}

{- |
>>> import Data.Bitraversable(Bitraversable(bitraverse))
>>> bitraverse (\e -> [e, e+1]) (\a -> [a, a*2]) (Failure 1 :: Validation Int Int)
[Failure 1,Failure 2]

>>> bitraverse (\e -> [e, e+1]) (\a -> [a, a*2]) (Success 3 :: Validation Int Int)
[Success 3,Success 6]
-}
instance Bitraversable Validation where
  bitraverse _ g (Success a) =
    Success <$> g a
  bitraverse f _ (Failure e) =
    Failure <$> f e
  {-# INLINE bitraverse #-}

instance Bitraversable1 Validation where
  bitraverse1 f _ (Failure e) = Failure <$> f e
  bitraverse1 _ g (Success a) = Success <$> g a
  {-# INLINE bitraverse1 #-}

{- | First 'Success' wins; two 'Failure's are combined with '<>'.

>>> Failure ["e1"] <> Failure ["e2"] :: Validation [String] Int
Failure ["e1","e2"]

>>> Failure ["e1"] <> Success 2 :: Validation [String] Int
Success 2

>>> Success 1 <> Failure ["e2"] :: Validation [String] Int
Success 1

>>> Success 1 <> Success 2 :: Validation [String] Int
Success 1
-}
instance (Semigroup e) => Semigroup (Validation e a) where
  Failure e1 <> Failure e2 = Failure (e1 <> e2)
  Failure _ <> Success a = Success a
  Success a <> _ = Success a
  {-# INLINE (<>) #-}

{- |
>>> mempty :: Validation [String] Int
Failure []
-}
instance (Monoid e) => Monoid (Validation e a) where
  mempty =
    Failure mempty
  {-# INLINE mempty #-}

{- |
>>> import Data.Bifunctor.Swap(Swap(swap))
>>> swap (Failure "err" :: Validation String Int)
Success "err"

>>> swap (Success 1 :: Validation String Int)
Failure 1
-}
instance Swap Validation where
  swap v =
    case v of
      Failure e -> Success e
      Success a -> Failure a
  {-# INLINE swap #-}

instance Assoc Validation where
  assoc (Failure (Failure a)) = Failure a
  assoc (Failure (Success b)) = Success (Failure b)
  assoc (Success c) = Success (Success c)
  {-# INLINE assoc #-}
  unassoc (Failure a) = Failure (Failure a)
  unassoc (Success (Failure b)) = Failure (Success b)
  unassoc (Success (Success c)) = Success c
  {-# INLINE unassoc #-}

{- |
>>> import Control.DeepSeq(rnf)
>>> rnf (Success 1 :: Validation String Int)
()

>>> rnf (Failure "err" :: Validation String Int)
()
-}
instance (NFData e, NFData a) => NFData (Validation e a) where
  rnf v =
    case v of
      Failure e -> rnf e
      Success a -> rnf a
  {-# INLINE rnf #-}

instance Extend (Validation err) where
  extended _ (Failure e) = Failure e
  extended f w@(Success _) = Success (f w)
  {-# INLINE extended #-}

{- | Catamorphism for 'Validation'.

>>> foldValidation show show (Failure 1 :: Validation Int Int)
"1"

>>> foldValidation show show (Success 2 :: Validation Int Int)
"2"
-}
foldValidation :: (a -> x) -> (b -> x) -> Validation a b -> x
foldValidation f _ (Failure a) = f a
foldValidation _ s (Success b) = s b
{-# INLINE foldValidation #-}

{- | Polymorphic 'Prism' targeting the 'Failure' constructor.

>>> import Control.Lens((^?), review)
>>> review __Failure "err" :: Validation String Int
Failure "err"

>>> (Failure "err" :: Validation String Int) ^? __Failure
Just "err"

>>> (Success 1 :: Validation String Int) ^? __Failure
Nothing
-}
__Failure :: Prism (Validation a b) (Validation a' b) a a'
__Failure =
  prism
    Failure
    ( \case
        Failure a -> Right a
        Success b -> Left (Success b)
    )
{-# INLINE __Failure #-}

{- | Polymorphic 'Prism' targeting the 'Success' constructor.

>>> import Control.Lens((^?), review)
>>> review __Success 1 :: Validation String Int
Success 1

>>> (Success 1 :: Validation String Int) ^? __Success
Just 1

>>> (Failure "err" :: Validation String Int) ^? __Success
Nothing
-}
__Success :: Prism (Validation a b) (Validation a b') b b'
__Success =
  prism
    Success
    ( \case
        Failure a -> Left (Failure a)
        Success b -> Right b
    )
{-# INLINE __Success #-}

{- | Isomorphism between 'Validation' and 'Either'.

>>> import Control.Lens(view)
>>> view either (Failure "err" :: Validation String Int)
Left "err"

>>> view either (Success 1 :: Validation String Int)
Right 1
-}
either :: Iso (Validation a b) (Validation a' b') (Either a b) (Either a' b')
either =
  iso
    (foldValidation Left Right)
    (Either.either Failure Success)
{-# INLINE either #-}

{- | Isomorphism between @Validation a a@ and @(Bool, a)@, where 'False' corresponds to 'Failure'.

>>> import Control.Lens(view)
>>> view codiagonal (Failure "x" :: Validation String String)
(False,"x")

>>> view codiagonal (Success "x" :: Validation String String)
(True,"x")
-}
codiagonal :: Iso (Validation a a) (Validation a' a') (Bool, a) (Bool, a')
codiagonal =
  iso
    (foldValidation (False,) (True,))
    (\(p, a) -> bool (Failure a) (Success a) p)
{-# INLINE codiagonal #-}

-- | Class for types that have a 'Getter' to a 'Validation'.
class GetValidation s err a | s -> err a where
  getValidation :: Getter s (Validation err a)

instance GetValidation (Validation err a) err a where
  getValidation = id
  {-# INLINE getValidation #-}

{- |
>>> import Control.Lens(view)
>>> view getValidation (Left "err" :: Either String Int)
Failure "err"

>>> view getValidation (Right 1 :: Either String Int)
Success 1
-}
instance GetValidation (Either err a) err a where
  getValidation = from Data.Validation.Validation.either
  {-# INLINE getValidation #-}

-- | Class for types that have a 'Lens'' to a 'Validation' (as generated by @makeClassy@).
class (GetValidation s err a) => HasValidation s err a | s -> err a where
  validation :: Lens' s (Validation err a)

instance HasValidation (Validation err a) err a where
  validation = id
  {-# INLINE validation #-}

{- |
>>> import Control.Lens(view, set)
>>> view validation (Left "err" :: Either String Int)
Failure "err"

>>> set validation (Success 2 :: Validation String Int) (Left "err" :: Either String Int)
Right 2
-}
instance HasValidation (Either err a) err a where
  validation = from Data.Validation.Validation.either
  {-# INLINE validation #-}

-- | Class for types that have a 'Review' to a 'Validation'.
class ReviewValidation s err a | s -> err a where
  reviewValidation :: Review s (Validation err a)
  reviewFailure :: Review s err
  reviewFailure = reviewValidation . reviewFailure
  {-# INLINE reviewFailure #-}
  reviewSuccess :: Review s a
  reviewSuccess = reviewValidation . reviewSuccess
  {-# INLINE reviewSuccess #-}

instance ReviewValidation (Validation err a) err a where
  reviewValidation = id
  {-# INLINE reviewValidation #-}
  reviewFailure = unto Failure
  {-# INLINE reviewFailure #-}
  reviewSuccess = unto Success
  {-# INLINE reviewSuccess #-}

{- |
>>> import Control.Lens((#))
>>> reviewValidation # (Failure "err" :: Validation String Int) :: Either String Int
Left "err"

>>> reviewValidation # (Success 1 :: Validation String Int) :: Either String Int
Right 1
-}
instance ReviewValidation (Either err a) err a where
  reviewValidation = from Data.Validation.Validation.either
  {-# INLINE reviewValidation #-}

-- | Class for types that have a 'Prism'' to a 'Validation' (as generated by @makeClassyPrisms@).
class (ReviewValidation s err a) => AsValidation s err a | s -> err a where
  _Validation :: Prism' s (Validation err a)
  _Failure :: Prism' s err
  _Failure = _Validation . _Failure
  {-# INLINE _Failure #-}
  _Success :: Prism' s a
  _Success = _Validation . _Success
  {-# INLINE _Success #-}

instance AsValidation (Validation err a) err a where
  _Validation = id
  {-# INLINE _Validation #-}
  _Failure = __Failure
  {-# INLINE _Failure #-}
  _Success = __Success
  {-# INLINE _Success #-}

{- |
>>> import Control.Lens((^?), (#))
>>> _Validation # (Failure "err" :: Validation String Int) :: Either String Int
Left "err"

>>> (Left "err" :: Either String Int) ^? _Validation
Just (Failure "err")

>>> (Right 1 :: Either String Int) ^? _Validation
Just (Success 1)
-}
instance AsValidation (Either err a) err a where
  _Validation = from Data.Validation.Validation.either
  {-# INLINE _Validation #-}
