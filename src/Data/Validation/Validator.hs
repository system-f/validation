{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Validation.Validator (
  -- * Accumulating, Bifunctor parameter order
  Validator (..),

  -- * Accumulating, Profunctor parameter order
  ValidatorProfunctor (..),

  -- * Short-circuiting monad, MonadTrans parameter order
  ValidatorMonadT (..),
  ValidatorMonad,

  -- * Short-circuiting monad, Profunctor parameter order
  ValidatorMonadProfunctorT (..),
  ValidatorMonadProfunctor,

  -- * Optics — Validator

  -- ** Classy lenses
  GetValidator (..),
  HasValidator (..),

  -- ** Classy prisms
  ReviewValidator (..),
  AsValidator (..),

  -- * Optics — ValidatorProfunctor

  -- ** Classy lenses
  GetValidatorProfunctor (..),
  HasValidatorProfunctor (..),

  -- ** Classy prisms
  ReviewValidatorProfunctor (..),
  AsValidatorProfunctor (..),

  -- * Optics — ValidatorMonadT

  -- ** Classy lenses
  GetValidatorMonadT (..),
  HasValidatorMonadT (..),

  -- ** Classy prisms
  ReviewValidatorMonadT (..),
  AsValidatorMonadT (..),

  -- * Optics — ValidatorMonadProfunctorT

  -- ** Classy lenses
  GetValidatorMonadProfunctorT (..),
  HasValidatorMonadProfunctorT (..),

  -- ** Classy prisms
  ReviewValidatorMonadProfunctorT (..),
  AsValidatorMonadProfunctorT (..),
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Arrow (arr, first), ArrowApply (app), ArrowChoice (left, right), ArrowPlus ((<+>)), ArrowZero (zeroArrow))
import Control.Category (Category (..))
import Control.Lens (Getter, Lens', Prism', Review, Rewrapped, Wrapped (_Wrapped', type Unwrapped), unto)
import Control.Lens.Iso (iso)
import Control.Monad (MonadPlus, ap, (>=>))
import Control.Monad.Cont.Class (MonadCont (callCC))
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader (ask, local, reader))
import Control.Monad.State.Class (MonadState (get, put, state))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer.Class (MonadWriter (listen, pass, tell, writer))
import Control.Selective (Selective (..), selectM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bifunctor.Swap (Swap (..))
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Bind.Trans (BindTrans (liftB))
import Data.Functor.Extend (Extend (extended))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Plus (Plus (zero))
import Data.Profunctor (Choice (left', right'), Profunctor (dimap, lmap, rmap), Strong (first', second'))
import Data.Profunctor.Sieve (Sieve (sieve))
import Data.Profunctor.Traversing (Traversing (traverse', wander))
import Data.Semigroupoid (Semigroupoid (o))
import Data.Validation.Validation (Validation (..))
import Data.Validation.ValidationMonad (ValidationMonadT (..), liftValidationMonadT)
import GHC.Generics (Generic)
import Prelude hiding (id, (.))

{- $setup
>>> import Data.Validation.Validation(Validation(..))
>>> import Data.Validation.ValidationMonad(ValidationMonadT(..))
>>> import Data.Validation.Validator
>>> import Data.Functor.Identity(Identity(..))
>>> import Data.Functor.Alt(Alt((<!>)))
>>> import Data.Functor.Apply(Apply((<.>)))
>>> import Data.Functor.Bind(Bind((>>-)))
>>> import Data.Functor.Extend(Extend(extended))
>>> import Data.Functor.Plus(Plus(zero))
>>> import Data.Bifunctor(Bifunctor(bimap))
>>> import Data.Bifunctor.Swap(Swap(swap))
>>> import Data.Profunctor(Profunctor(dimap, lmap, rmap), Strong(first', second'), Choice(left', right'))
>>> import Data.Profunctor.Sieve(Sieve(sieve))
>>> import Data.Profunctor.Traversing(Traversing(traverse'))
>>> import Data.Semigroupoid(Semigroupoid(o))
>>> import Control.Category(id, (.))
>>> import Control.Arrow(Arrow(arr, first), ArrowApply(app), ArrowChoice(left, right), ArrowZero(zeroArrow), ArrowPlus((<+>)))
>>> import Control.Applicative(Alternative(empty))
>>> import Control.Selective(Selective(select))
>>> import Control.Monad.Error.Class(MonadError(throwError, catchError))
>>> import Control.Monad.Trans.Class(MonadTrans(lift))
>>> import Control.Lens(view, review, _Wrapped', (^?))
>>> import Prelude hiding (id, (.))
>>> :set -w
>>> let runVP (ValidatorProfunctor f) = f
>>> let vpOk x = ValidatorProfunctor (\_ -> Success x) :: ValidatorProfunctor [String] Int Int
>>> let vpErr e = ValidatorProfunctor (\_ -> Failure e) :: ValidatorProfunctor [String] Int Int
>>> let vpFromInput = ValidatorProfunctor (\x -> Success (x + 1)) :: ValidatorProfunctor [String] Int Int
>>> let runVMP v x = let ValidatorMonadProfunctorT f = v in let ValidationMonadT (Identity r) = f x in r
>>> let vmpOk a = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (a x)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let vmpSucc a = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Success a))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let vmpErr e = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Failure e))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let vmpFail = vmpErr ["fail"]
-}

-- ========================================
-- Validator (accumulating, Bifunctor order)
-- ========================================

{- | A validator that applies a function @x -> Validation err a@.
The 'Applicative' instance /accumulates/ errors using 'Semigroup', like 'Validation'.

>>> let Validator f = Validator (\x -> if x > 0 then Success x else Failure ["not positive"]) :: Validator Int [String] Int
>>> f 5
Success 5

>>> f (-1)
Failure ["not positive"]
-}
newtype Validator x err a = Validator (x -> Validation err a)
  deriving (Generic)

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let v = Validator (\x -> Success (x + 1)) :: Validator Int [String] Int
>>> (view _Wrapped' v) 10
Success 11
-}
instance Wrapped (Validator x err a) where
  type Unwrapped (Validator x err a) = x -> Validation err a
  _Wrapped' = iso (\(Validator f) -> f) Validator
  {-# INLINE _Wrapped' #-}

instance Rewrapped (Validator x err a) (Validator x' err' b)

{- |
>>> let Validator f = fmap (+1) (Validator Success :: Validator Int [String] Int)
>>> f 10
Success 11

>>> let Validator f = fmap (+1) (Validator (\_ -> Failure ["err"]) :: Validator Int [String] Int)
>>> f 10
Failure ["err"]
-}
instance Functor (Validator x err) where
  fmap f (Validator g) = Validator (fmap (fmap f) g)
  {-# INLINE fmap #-}

{- | Accumulates errors using 'Semigroup'.

>>> import Data.Functor.Apply(Apply((<.>)))
>>> let Validator f = Validator (\_ -> Success (+1)) <.> (Validator Success :: Validator Int [String] Int)
>>> f 10
Success 11

>>> let Validator f = (Validator (\_ -> Failure ["e1"]) :: Validator Int [String] (Int -> Int)) <.> (Validator (\_ -> Failure ["e2"]) :: Validator Int [String] Int)
>>> f 0
Failure ["e1","e2"]
-}
instance (Semigroup err) => Apply (Validator x err) where
  Validator f <.> Validator g = Validator (\x -> f x <.> g x)
  {-# INLINE (<.>) #-}

{- | Accumulates errors using 'Semigroup'.

>>> let Validator f = pure 42 :: Validator Int [String] Int
>>> f 0
Success 42

>>> let Validator f = pure (+) <*> (Validator (\_ -> Failure ["e1"]) :: Validator Int [String] Int) <*> (Validator (\_ -> Failure ["e2"]) :: Validator Int [String] Int)
>>> f 0
Failure ["e1","e2"]
-}
instance (Semigroup err) => Applicative (Validator x err) where
  pure a = Validator (\_ -> Success a)
  {-# INLINE pure #-}
  Validator f <*> Validator g = Validator (\x -> f x <.> g x)
  {-# INLINE (<*>) #-}

{- | First success wins; two failures accumulate.

>>> import Data.Functor.Alt(Alt((<!>)))
>>> let Validator f = (Validator (\_ -> Failure ["e1"]) :: Validator Int [String] Int) <!> Validator (\_ -> Success 2)
>>> f 0
Success 2

>>> let Validator f = (Validator (\_ -> Failure ["e1"]) :: Validator Int [String] Int) <!> Validator (\_ -> Failure ["e2"])
>>> f 0
Failure ["e1","e2"]
-}
instance (Semigroup err) => Alt (Validator x err) where
  Validator f <!> Validator g = Validator (\x -> f x <!> g x)
  {-# INLINE (<!>) #-}

{- |
>>> import Data.Functor.Alt(Alt((<!>)))
>>> import Data.Functor.Plus(Plus(zero))
>>> let Validator f = (zero :: Validator Int [String] Int) <!> Validator (\_ -> Success 1)
>>> f 0
Success 1
-}
instance (Monoid err) => Plus (Validator x err) where
  zero = Validator (\_ -> Failure mempty)
  {-# INLINE zero #-}

{- |
>>> let Validator f = (empty :: Validator Int [String] Int) <|> Validator (\_ -> Success 1)
>>> f 0
Success 1
-}
instance (Monoid err) => Alternative (Validator x err) where
  empty = zero
  {-# INLINE empty #-}
  (<|>) = (<!>)
  {-# INLINE (<|>) #-}

{- |
>>> import Control.Selective(Selective(select))
>>> let Validator f = select (Validator (\_ -> Success (Right 1)) :: Validator Int [String] (Either Int Int)) (pure (+1))
>>> f 0
Success 1

>>> let Validator f = select (Validator (\_ -> Success (Left 1)) :: Validator Int [String] (Either Int Int)) (pure (+1))
>>> f 0
Success 2
-}
instance (Semigroup err) => Selective (Validator x err) where
  select (Validator f) (Validator g) = Validator (\x -> select (f x) (g x))
  {-# INLINE select #-}

{- |
>>> import Data.Bifunctor(Bifunctor(bimap))
>>> let Validator f = bimap (map (++ "!")) (+1) (Validator Success :: Validator Int [String] Int)
>>> f 10
Success 11

>>> let Validator f = bimap (map (++ "!")) (+1) (Validator (\_ -> Failure ["err"]) :: Validator Int [String] Int)
>>> f 0
Failure ["err!"]
-}
instance Bifunctor (Validator x) where
  bimap f g (Validator h) = Validator (bimap f g . h)
  {-# INLINE bimap #-}

{- |
>>> import Data.Bifunctor.Swap(Swap(swap))
>>> let Validator f = swap (Validator (\_ -> Failure "err") :: Validator Int String Int)
>>> f 0
Success "err"

>>> let Validator f = swap (Validator (\_ -> Success 1) :: Validator Int String Int)
>>> f 0
Failure 1
-}
instance Swap (Validator x) where
  swap (Validator f) = Validator (swap . f)
  {-# INLINE swap #-}

{- |
>>> import Data.Functor.Extend(Extend(extended))
>>> let Validator f = extended (\_ -> 42) (Validator (\_ -> Success 1) :: Validator Int [String] Int)
>>> f 0
Success 42
-}
instance Extend (Validator x err) where
  extended f w@(Validator _) = Validator (\_ -> Success (f w))
  {-# INLINE extended #-}

{- |
>>> let Validator f = (Validator (\_ -> Failure ["e1"]) :: Validator Int [String] Int) <> Validator (\_ -> Failure ["e2"])
>>> f 0
Failure ["e1","e2"]

>>> let Validator f = (Validator (\_ -> Success 1) :: Validator Int [String] Int) <> Validator (\_ -> Failure ["e2"])
>>> f 0
Success 1
-}
instance (Semigroup err) => Semigroup (Validator x err a) where
  Validator f <> Validator g = Validator (\x -> f x <> g x)
  {-# INLINE (<>) #-}

{- |
>>> let Validator f = mempty :: Validator Int [String] Int
>>> f 0
Failure []
-}
instance (Monoid err) => Monoid (Validator x err a) where
  mempty = Validator (const mempty)
  {-# INLINE mempty #-}

{- | Class for types that have a 'Getter' to a 'Validator'.

>>> import Control.Lens(view)
>>> let Validator f = view getValidator (Validator (\_ -> Success 1) :: Validator Int [String] Int)
>>> f 0
Success 1
-}
class GetValidator s x err a | s -> x err a where
  getValidator :: Getter s (Validator x err a)

instance GetValidator (Validator x err a) x err a where
  getValidator = id
  {-# INLINE getValidator #-}

{- | Class for types that have a 'Lens'' to a 'Validator'.

>>> import Control.Lens(view)
>>> let Validator f = view validator (Validator (\_ -> Success 1) :: Validator Int [String] Int)
>>> f 0
Success 1
-}
class (GetValidator s x err a) => HasValidator s x err a | s -> x err a where
  validator :: Lens' s (Validator x err a)

instance HasValidator (Validator x err a) x err a where
  validator = id
  {-# INLINE validator #-}

-- | Class for types that have a 'Review' to a 'Validator'.
class ReviewValidator s x err a | s -> x err a where
  reviewValidator :: Review s (Validator x err a)

instance ReviewValidator (Validator x err a) x err a where
  reviewValidator = unto id
  {-# INLINE reviewValidator #-}

-- | Class for types that have a 'Prism'' to a 'Validator'.
class (ReviewValidator s x err a) => AsValidator s x err a | s -> x err a where
  _Validator :: Prism' s (Validator x err a)

instance AsValidator (Validator x err a) x err a where
  _Validator = id
  {-# INLINE _Validator #-}

-- =============================================
-- ValidatorProfunctor (accumulating, Profunctor order)
-- =============================================

{- | A validator function @x -> Validation err a@ with @err@ as the
outermost parameter, enabling 'Profunctor' and related instances.

>>> runVP (ValidatorProfunctor (\x -> Success (x * 2))) 5
Success 10

>>> runVP (ValidatorProfunctor (\_ -> Failure ["bad"])) 5
Failure ["bad"]
-}
newtype ValidatorProfunctor err x a = ValidatorProfunctor (x -> Validation err a)
  deriving (Generic)

{- |
>>> view _Wrapped' vpFromInput $ 3
Success 4
-}
instance Wrapped (ValidatorProfunctor err x a) where
  type Unwrapped (ValidatorProfunctor err x a) = x -> Validation err a
  _Wrapped' = iso (\(ValidatorProfunctor f) -> f) ValidatorProfunctor
  {-# INLINE _Wrapped' #-}

instance Rewrapped (ValidatorProfunctor err x a) (ValidatorProfunctor err' x' b)

{- |
>>> runVP (fmap (+10) vpFromInput) 3
Success 14

>>> runVP (fmap (+10) (vpErr ["e"])) 3
Failure ["e"]
-}
instance Functor (ValidatorProfunctor err x) where
  fmap f (ValidatorProfunctor g) = ValidatorProfunctor (fmap (fmap f) g)
  {-# INLINE fmap #-}

{- | Accumulates errors from both sides.

>>> runVP (ValidatorProfunctor (\_ -> Success (+1)) <.> vpOk 2 :: ValidatorProfunctor [String] Int Int) 0
Success 3

>>> runVP (ValidatorProfunctor (\_ -> Failure ["e1"]) <.> ValidatorProfunctor (\_ -> Failure ["e2"]) :: ValidatorProfunctor [String] Int Int) 0
Failure ["e1","e2"]

>>> runVP (ValidatorProfunctor (\_ -> Failure ["e1"]) <.> vpOk 2 :: ValidatorProfunctor [String] Int Int) 0
Failure ["e1"]

>>> runVP (ValidatorProfunctor (\_ -> Success (+1)) <.> vpErr ["e2"] :: ValidatorProfunctor [String] Int Int) 0
Failure ["e2"]
-}
instance (Semigroup err) => Apply (ValidatorProfunctor err x) where
  ValidatorProfunctor f <.> ValidatorProfunctor g = ValidatorProfunctor (\x -> f x <.> g x)
  {-# INLINE (<.>) #-}

{- | 'pure' ignores the input, '<*>' accumulates errors.

>>> runVP (pure 42 :: ValidatorProfunctor [String] Int Int) 0
Success 42

>>> runVP (pure (+1) <*> pure 2 :: ValidatorProfunctor [String] Int Int) 0
Success 3

>>> runVP (ValidatorProfunctor (\_ -> Failure ["e1"]) <*> ValidatorProfunctor (\_ -> Failure ["e2"]) :: ValidatorProfunctor [String] Int Int) 0
Failure ["e1","e2"]
-}
instance (Semigroup err) => Applicative (ValidatorProfunctor err x) where
  pure a = ValidatorProfunctor (\_ -> Success a)
  {-# INLINE pure #-}
  ValidatorProfunctor f <*> ValidatorProfunctor g = ValidatorProfunctor (\x -> f x <.> g x)
  {-# INLINE (<*>) #-}

{- | First success wins; two failures accumulate.

>>> runVP (vpOk 1 <!> vpOk 2) 0
Success 1

>>> runVP (vpErr ["e1"] <!> vpOk 2) 0
Success 2

>>> runVP (vpOk 1 <!> vpErr ["e2"]) 0
Success 1

>>> runVP (vpErr ["e1"] <!> vpErr ["e2"]) 0
Failure ["e1","e2"]
-}
instance (Semigroup err) => Alt (ValidatorProfunctor err x) where
  ValidatorProfunctor f <!> ValidatorProfunctor g = ValidatorProfunctor (\x -> f x <!> g x)
  {-# INLINE (<!>) #-}

{- |
>>> runVP (zero :: ValidatorProfunctor [String] Int Int) 0
Failure []
-}
instance (Monoid err) => Plus (ValidatorProfunctor err x) where
  zero = ValidatorProfunctor (\_ -> Failure mempty)
  {-# INLINE zero #-}

{- |
>>> runVP (empty :: ValidatorProfunctor [String] Int Int) 0
Failure []

>>> runVP (vpErr ["e1"] <|> vpOk 2) 0
Success 2
-}
instance (Monoid err) => Alternative (ValidatorProfunctor err x) where
  empty = zero
  {-# INLINE empty #-}
  (<|>) = (<!>)
  {-# INLINE (<|>) #-}

{- |
>>> runVP (select (pure (Right 1)) (pure (+1)) :: ValidatorProfunctor [String] Int Int) 0
Success 1

>>> runVP (select (pure (Left 1)) (pure (+1)) :: ValidatorProfunctor [String] Int Int) 0
Success 2

>>> runVP (select (ValidatorProfunctor (\_ -> Failure ["e1"])) (pure (+1)) :: ValidatorProfunctor [String] Int Int) 0
Failure ["e1"]
-}
instance (Semigroup err) => Selective (ValidatorProfunctor err x) where
  select (ValidatorProfunctor f) (ValidatorProfunctor g) = ValidatorProfunctor (\x -> select (f x) (g x))
  {-# INLINE select #-}

{- | Contravariant in @x@, covariant in @a@.

>>> runVP (dimap (*2) (+10) vpFromInput) 3
Success 17

>>> runVP (lmap (*2) vpFromInput) 3
Success 7

>>> runVP (rmap (+10) vpFromInput) 3
Success 14
-}
instance Profunctor (ValidatorProfunctor err) where
  dimap f g (ValidatorProfunctor h) = ValidatorProfunctor (fmap g . h . f)
  {-# INLINE dimap #-}
  lmap f (ValidatorProfunctor h) = ValidatorProfunctor (h . f)
  {-# INLINE lmap #-}
  rmap g (ValidatorProfunctor h) = ValidatorProfunctor (fmap g . h)
  {-# INLINE rmap #-}

{- |
>>> runVP (first' vpFromInput) (3, "tag")
Success (4,"tag")

>>> runVP (second' vpFromInput) ("tag", 3)
Success ("tag",4)
-}
instance Strong (ValidatorProfunctor err) where
  first' (ValidatorProfunctor f) = ValidatorProfunctor (\(a, c) -> fmap (,c) (f a))
  {-# INLINE first' #-}
  second' (ValidatorProfunctor f) = ValidatorProfunctor (\(c, a) -> fmap (c,) (f a))
  {-# INLINE second' #-}

{- |
>>> runVP (left' vpFromInput) (Left 3)
Success (Left 4)

>>> runVP (left' vpFromInput) (Right "x" :: Either Int String)
Success (Right "x")

>>> runVP (right' vpFromInput) (Right 3)
Success (Right 4)

>>> runVP (right' vpFromInput) (Left "x" :: Either String Int)
Success (Left "x")
-}
instance (Semigroup err) => Choice (ValidatorProfunctor err) where
  left' (ValidatorProfunctor f) = ValidatorProfunctor (either (fmap Left . f) (pure . Right))
  {-# INLINE left' #-}
  right' (ValidatorProfunctor f) = ValidatorProfunctor (either (pure . Left) (fmap Right . f))
  {-# INLINE right' #-}

{- |
>>> runVP (traverse' vpFromInput) [1, 2, 3]
Success [2,3,4]
-}
instance (Semigroup err) => Traversing (ValidatorProfunctor err) where
  traverse' (ValidatorProfunctor f) = ValidatorProfunctor (traverse f)
  {-# INLINE traverse' #-}
  wander t (ValidatorProfunctor f) = ValidatorProfunctor (t f)
  {-# INLINE wander #-}

{- |
>>> sieve vpFromInput 3
Success 4

>>> sieve (vpErr ["e"]) 0
Failure ["e"]
-}
instance Sieve (ValidatorProfunctor err) (Validation err) where
  sieve (ValidatorProfunctor f) = f
  {-# INLINE sieve #-}

{- |
>>> runVP (extended (\_ -> 42) vpFromInput) 0
Success 42
-}
instance Extend (ValidatorProfunctor err x) where
  extended f w@(ValidatorProfunctor _) = ValidatorProfunctor (\_ -> Success (f w))
  {-# INLINE extended #-}

{- |
>>> runVP (vpOk 1 <> vpOk 2) 0
Success 1

>>> runVP (vpErr ["e1"] <> vpErr ["e2"]) 0
Failure ["e1","e2"]

>>> runVP (vpErr ["e1"] <> vpOk 2) 0
Success 2
-}
instance (Semigroup err) => Semigroup (ValidatorProfunctor err x a) where
  ValidatorProfunctor f <> ValidatorProfunctor g = ValidatorProfunctor (\x -> f x <> g x)
  {-# INLINE (<>) #-}

{- |
>>> runVP (mempty :: ValidatorProfunctor [String] Int Int) 0
Failure []
-}
instance (Monoid err) => Monoid (ValidatorProfunctor err x a) where
  mempty = ValidatorProfunctor (const mempty)
  {-# INLINE mempty #-}

{- |
>>> runVP (view getValidatorProfunctor vpFromInput) 3
Success 4
-}
class GetValidatorProfunctor s err x a | s -> err x a where
  getValidatorProfunctor :: Getter s (ValidatorProfunctor err x a)

instance GetValidatorProfunctor (ValidatorProfunctor err x a) err x a where
  getValidatorProfunctor = id
  {-# INLINE getValidatorProfunctor #-}

{- |
>>> runVP (view validatorProfunctor vpFromInput) 3
Success 4
-}
class (GetValidatorProfunctor s err x a) => HasValidatorProfunctor s err x a | s -> err x a where
  validatorProfunctor :: Lens' s (ValidatorProfunctor err x a)

instance HasValidatorProfunctor (ValidatorProfunctor err x a) err x a where
  validatorProfunctor = id
  {-# INLINE validatorProfunctor #-}

{- |
>>> runVP (review reviewValidatorProfunctor vpFromInput) 3
Success 4
-}
class ReviewValidatorProfunctor s err x a | s -> err x a where
  reviewValidatorProfunctor :: Review s (ValidatorProfunctor err x a)

instance ReviewValidatorProfunctor (ValidatorProfunctor err x a) err x a where
  reviewValidatorProfunctor = unto id
  {-# INLINE reviewValidatorProfunctor #-}

{- |
>>> let v = review _ValidatorProfunctor vpFromInput :: ValidatorProfunctor [String] Int Int
>>> runVP v 3
Success 4
-}
class (ReviewValidatorProfunctor s err x a) => AsValidatorProfunctor s err x a | s -> err x a where
  _ValidatorProfunctor :: Prism' s (ValidatorProfunctor err x a)

instance AsValidatorProfunctor (ValidatorProfunctor err x a) err x a where
  _ValidatorProfunctor = id
  {-# INLINE _ValidatorProfunctor #-}

-- ==============================================
-- ValidatorMonadT (short-circuiting, MonadTrans order)
-- ==============================================

{- | A validator with short-circuiting 'Monad' and 'MonadTrans' instances.
The parameter order @x err f a@ enables 'MonadTrans' on @ValidatorMonadT x err@.

>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let v = ValidatorMonadT (\x -> ValidationMonadT (Identity (if x > 0 then Success x else Failure ["non-positive"])))
>>> let ValidatorMonadT f = v in let ValidationMonadT (Identity r) = f 5 in r
Success 5

>>> let ValidatorMonadT f = v in let ValidationMonadT (Identity r) = f (-1) in r
Failure ["non-positive"]
-}
newtype ValidatorMonadT x err f a = ValidatorMonadT (x -> ValidationMonadT err f a)
  deriving (Generic)

-- | @ValidatorMonad x err a@ is @ValidatorMonadT x err Identity a@.
type ValidatorMonad x err a = ValidatorMonadT x err Identity a

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Control.Lens (view, _Wrapped')
>>> let v = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int String Identity Int
>>> view _Wrapped' v $ 0
ValidationMonadT (Identity (Success 1))
-}
instance Wrapped (ValidatorMonadT x err f a) where
  type Unwrapped (ValidatorMonadT x err f a) = x -> ValidationMonadT err f a
  _Wrapped' = iso (\(ValidatorMonadT f) -> f) ValidatorMonadT
  {-# INLINE _Wrapped' #-}

instance Rewrapped (ValidatorMonadT x err f a) (ValidatorMonadT x' err' f' b)

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let v = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = fmap (+1) v in let ValidationMonadT (Identity r) = f 0 in r
Success 2
-}
instance (Functor f) => Functor (ValidatorMonadT x err f) where
  fmap f (ValidatorMonadT g) = ValidatorMonadT (fmap (fmap f) g)
  {-# INLINE fmap #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Data.Functor.Apply ((<.>))
>>> let f = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success (+1)))) :: ValidatorMonadT Int [String] Identity (Int -> Int)
>>> let a = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 2))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT g = f <.> a in let ValidationMonadT (Identity r) = g 0 in r
Success 3
-}
instance (Monad f) => Apply (ValidatorMonadT x err f) where
  (<.>) = ap
  {-# INLINE (<.>) #-}

{- | Short-circuits on first failure (unlike 'Validation' which accumulates).

>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let e1 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadT Int [String] Identity (Int -> Int)
>>> let e2 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e2"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = e1 <*> e2 in let ValidationMonadT (Identity r) = f 0 in r
Failure ["e1"]
-}
instance (Monad f) => Applicative (ValidatorMonadT x err f) where
  pure a = ValidatorMonadT (\_ -> pure a)
  {-# INLINE pure #-}
  ValidatorMonadT f <*> ValidatorMonadT g = ValidatorMonadT (\x -> f x <*> g x)
  {-# INLINE (<*>) #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Data.Functor.Bind ((>>-))
>>> let v = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = v >>- \a -> ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success (a + 1)))) in let ValidationMonadT (Identity r) = f 0 in r
Success 2
-}
instance (Monad f) => Bind (ValidatorMonadT x err f) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

{- | Short-circuits on 'Failure'.

>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let e1 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let e2 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e2"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = e1 >> e2 in let ValidationMonadT (Identity r) = f 0 in r
Failure ["e1"]
-}
instance (Monad f) => Monad (ValidatorMonadT x err f) where
  ValidatorMonadT f >>= k = ValidatorMonadT (\x -> f x >>= \a -> let ValidatorMonadT g = k a in g x)
  {-# INLINE (>>=) #-}

instance (Monad f, MonadFail f) => MonadFail (ValidatorMonadT x err f) where
  fail = ValidatorMonadT . const . liftValidationMonadT . Prelude.fail
  {-# INLINE fail #-}

{- | First success wins; two failures accumulate.

>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Data.Functor.Alt ((<!>))
>>> let e1 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let ok = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 2))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = e1 <!> ok in let ValidationMonadT (Identity r) = f 0 in r
Success 2
-}
instance (Monad f, Semigroup err) => Alt (ValidatorMonadT x err f) where
  ValidatorMonadT f <!> ValidatorMonadT g = ValidatorMonadT (\x -> f x <!> g x)
  {-# INLINE (<!>) #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Data.Functor.Alt ((<!>))
>>> import Data.Functor.Plus (zero)
>>> let ok = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = (zero :: ValidatorMonadT Int [String] Identity Int) <!> ok in let ValidationMonadT (Identity r) = f 0 in r
Success 1
-}
instance (Monad f, Monoid err) => Plus (ValidatorMonadT x err f) where
  zero = ValidatorMonadT (const zero)
  {-# INLINE zero #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let ok = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = empty <|> ok in let ValidationMonadT (Identity r) = f 0 in r
Success 1
-}
instance (Monad f, Monoid err) => Alternative (ValidatorMonadT x err f) where
  empty = zero
  {-# INLINE empty #-}
  (<|>) = (<!>)
  {-# INLINE (<|>) #-}

instance (Monad f, Monoid err) => MonadPlus (ValidatorMonadT x err f)

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Control.Selective (select)
>>> let ok a = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success a)))
>>> let ValidatorMonadT f = select (ok (Left (1 :: Int))) (ok (+1)) :: ValidatorMonadT Int [String] Identity Int in let ValidationMonadT (Identity r) = f 0 in r
Success 2
-}
instance (Monad f) => Selective (ValidatorMonadT x err f) where
  select = selectM
  {-# INLINE select #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Data.Functor.Extend (extended)
>>> let v = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = extended (\_ -> 42) v in let ValidationMonadT (Identity r) = f 0 in r
Success 42
-}
instance (Monad f) => Extend (ValidatorMonadT x err f) where
  extended f w@(ValidatorMonadT _) = ValidatorMonadT (\_ -> pure (f w))
  {-# INLINE extended #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let e1 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let e2 = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e2"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = e1 <> e2 in let ValidationMonadT (Identity r) = f 0 in r
Failure ["e1","e2"]
-}
instance (Applicative f, Semigroup err) => Semigroup (ValidatorMonadT x err f a) where
  ValidatorMonadT f <> ValidatorMonadT g = ValidatorMonadT (\x -> f x <> g x)
  {-# INLINE (<>) #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> let ValidatorMonadT f = mempty :: ValidatorMonadT Int [String] Identity Int in let ValidationMonadT (Identity r) = f 0 in r
Failure []
-}
instance (Applicative f, Monoid err) => Monoid (ValidatorMonadT x err f a) where
  mempty = ValidatorMonadT (const mempty)
  {-# INLINE mempty #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Control.Monad.Trans.Class (lift)
>>> let ValidatorMonadT f = lift (Identity 42) :: ValidatorMonadT Int [String] Identity Int in let ValidationMonadT (Identity r) = f 0 in r
Success 42
-}
instance MonadTrans (ValidatorMonadT x err) where
  lift = ValidatorMonadT . const . liftValidationMonadT
  {-# INLINE lift #-}

instance BindTrans (ValidatorMonadT x err) where
  liftB = ValidatorMonadT . const . liftValidationMonadT
  {-# INLINE liftB #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Control.Monad.Error.Class (throwError, catchError)
>>> let ValidatorMonadT f = throwError ["oops"] :: ValidatorMonadT Int [String] Identity Int in let ValidationMonadT (Identity r) = f 0 in r
Failure ["oops"]

>>> let e = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Failure ["e"]))) :: ValidatorMonadT Int [String] Identity Int
>>> let ValidatorMonadT f = catchError e (\_ -> ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 99)))) in let ValidationMonadT (Identity r) = f 0 in r
Success 99
-}
instance (Monad f) => MonadError err (ValidatorMonadT x err f) where
  throwError e = ValidatorMonadT (\_ -> throwError e)
  {-# INLINE throwError #-}
  catchError (ValidatorMonadT f) h = ValidatorMonadT (\x -> catchError (f x) (\e -> let ValidatorMonadT g = h e in g x))
  {-# INLINE catchError #-}

instance (MonadIO f) => MonadIO (ValidatorMonadT x err f) where
  liftIO = ValidatorMonadT . const . liftValidationMonadT . liftIO
  {-# INLINE liftIO #-}

instance (MonadReader r f) => MonadReader r (ValidatorMonadT x err f) where
  ask = ValidatorMonadT (\_ -> liftValidationMonadT ask)
  {-# INLINE ask #-}
  local f (ValidatorMonadT g) = ValidatorMonadT (local f . g)
  {-# INLINE local #-}
  reader = ValidatorMonadT . const . liftValidationMonadT . reader
  {-# INLINE reader #-}

instance (MonadWriter w f) => MonadWriter w (ValidatorMonadT x err f) where
  writer = ValidatorMonadT . const . liftValidationMonadT . writer
  {-# INLINE writer #-}
  tell = ValidatorMonadT . const . liftValidationMonadT . tell
  {-# INLINE tell #-}
  listen (ValidatorMonadT f) = ValidatorMonadT (listen . f)
  {-# INLINE listen #-}
  pass (ValidatorMonadT f) = ValidatorMonadT (pass . f)
  {-# INLINE pass #-}

instance (MonadState s f) => MonadState s (ValidatorMonadT x err f) where
  get = ValidatorMonadT (\_ -> liftValidationMonadT get)
  {-# INLINE get #-}
  put = ValidatorMonadT . const . liftValidationMonadT . put
  {-# INLINE put #-}
  state = ValidatorMonadT . const . liftValidationMonadT . state
  {-# INLINE state #-}

instance (MonadCont f) => MonadCont (ValidatorMonadT x err f) where
  callCC f = ValidatorMonadT (\x -> callCC (\c -> let ValidatorMonadT g = f (\a -> ValidatorMonadT (\_ -> c a)) in g x))
  {-# INLINE callCC #-}

instance (MonadRWS r w s f) => MonadRWS r w s (ValidatorMonadT x err f)

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Control.Lens (view)
>>> let v = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int String Identity Int
>>> let ValidatorMonadT f = view getValidatorMonadT v in let ValidationMonadT (Identity r) = f 0 in r
Success 1
-}
class GetValidatorMonadT s x err f a | s -> x err f a where
  getValidatorMonadT :: Getter s (ValidatorMonadT x err f a)

instance GetValidatorMonadT (ValidatorMonadT x err f a) x err f a where
  getValidatorMonadT = id
  {-# INLINE getValidatorMonadT #-}

{- |
>>> import Data.Functor.Identity (Identity(..))
>>> import Data.Validation.Validation (Validation(..))
>>> import Data.Validation.ValidationMonad (ValidationMonadT(..))
>>> import Control.Lens (view)
>>> let v = ValidatorMonadT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadT Int String Identity Int
>>> let ValidatorMonadT f = view validatorMonadT v in let ValidationMonadT (Identity r) = f 0 in r
Success 1
-}
class (GetValidatorMonadT s x err f a) => HasValidatorMonadT s x err f a | s -> x err f a where
  validatorMonadT :: Lens' s (ValidatorMonadT x err f a)

instance HasValidatorMonadT (ValidatorMonadT x err f a) x err f a where
  validatorMonadT = id
  {-# INLINE validatorMonadT #-}

class ReviewValidatorMonadT s x err f a | s -> x err f a where
  reviewValidatorMonadT :: Review s (ValidatorMonadT x err f a)

instance ReviewValidatorMonadT (ValidatorMonadT x err f a) x err f a where
  reviewValidatorMonadT = unto id
  {-# INLINE reviewValidatorMonadT #-}

class (ReviewValidatorMonadT s x err f a) => AsValidatorMonadT s x err f a | s -> x err f a where
  _ValidatorMonadT :: Prism' s (ValidatorMonadT x err f a)

instance AsValidatorMonadT (ValidatorMonadT x err f a) x err f a where
  _ValidatorMonadT = id
  {-# INLINE _ValidatorMonadT #-}

-- =====================================================
-- ValidatorMonadProfunctorT (short-circuiting, Profunctor order)
-- =====================================================

{- | A profunctor validator with short-circuiting monadic semantics.

@ValidatorMonadProfunctorT err f x a@ wraps @x -> ValidationMonadT err f a@.
The @Applicative@ and @Monad@ instances short-circuit on the first 'Failure'.
@Category@ composition sequences validators, short-circuiting on the first failure.

>>> import Control.Lens(view, _Wrapped')
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = (view _Wrapped' v) 3
>>> r
Success 4
-}
newtype ValidatorMonadProfunctorT err f x a = ValidatorMonadProfunctorT (x -> ValidationMonadT err f a)
  deriving (Generic)

-- | @ValidatorMonadProfunctor@ is @ValidatorMonadProfunctorT@ specialised to 'Identity'.
type ValidatorMonadProfunctor err x a = ValidatorMonadProfunctorT err Identity x a

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> view _Wrapped' v 3
ValidationMonadT (Identity (Success 4))
-}
instance Wrapped (ValidatorMonadProfunctorT err f x a) where
  type Unwrapped (ValidatorMonadProfunctorT err f x a) = x -> ValidationMonadT err f a
  _Wrapped' = iso (\(ValidatorMonadProfunctorT f) -> f) ValidatorMonadProfunctorT
  {-# INLINE _Wrapped' #-}

instance Rewrapped (ValidatorMonadProfunctorT err f x a) (ValidatorMonadProfunctorT err' f' x' b)

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = (view _Wrapped' (fmap (*10) v)) 3
>>> r
Success 40
-}
instance (Functor f) => Functor (ValidatorMonadProfunctorT err f x) where
  fmap f (ValidatorMonadProfunctorT g) = ValidatorMonadProfunctorT (fmap (fmap f) g)
  {-# INLINE fmap #-}

{- | Short-circuiting: stops at the first 'Failure'.

>>> import Control.Lens(view, _Wrapped')
>>> import Data.Functor.Apply(Apply((<.>)))
>>> let f = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (+ x)))) :: ValidatorMonadProfunctorT [String] Identity Int (Int -> Int)
>>> let g = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x * 2)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = (view _Wrapped' (f <.> g)) 3
>>> r
Success 9
-}
instance (Monad f) => Apply (ValidatorMonadProfunctorT err f x) where
  (<.>) = ap
  {-# INLINE (<.>) #-}

{- | Short-circuiting: unlike 'Validation', does /not/ accumulate errors.

>>> import Control.Lens(view, _Wrapped')
>>> let ValidationMonadT (Identity r) = view _Wrapped' (pure 42 :: ValidatorMonadProfunctorT [String] Identity Int Int) 0
>>> r
Success 42
-}
instance (Monad f) => Applicative (ValidatorMonadProfunctorT err f x) where
  pure a = ValidatorMonadProfunctorT (\_ -> pure a)
  {-# INLINE pure #-}
  ValidatorMonadProfunctorT f <*> ValidatorMonadProfunctorT g = ValidatorMonadProfunctorT (\x -> f x <*> g x)
  {-# INLINE (<*>) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Functor.Bind(Bind((>>-)))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v >>- \a -> pure (a * 10)) 3
>>> r
Success 40
-}
instance (Monad f) => Bind (ValidatorMonadProfunctorT err f x) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v >>= \a -> pure (a * 10)) 3
>>> r
Success 40

>>> let f = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (f >>= \a -> pure (a * 10)) 3
>>> r
Failure ["e1"]
-}
instance (Monad f) => Monad (ValidatorMonadProfunctorT err f x) where
  ValidatorMonadProfunctorT f >>= k = ValidatorMonadProfunctorT (\x -> f x >>= \a -> let ValidatorMonadProfunctorT g = k a in g x)
  {-# INLINE (>>=) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Functor.Alt(Alt((<!>)))
>>> let v1 = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let v2 = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x * 2)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v1 <!> v2) 3
>>> r
Success 6
-}
instance (Monad f, Semigroup err) => Alt (ValidatorMonadProfunctorT err f x) where
  ValidatorMonadProfunctorT f <!> ValidatorMonadProfunctorT g = ValidatorMonadProfunctorT (\x -> f x <!> g x)
  {-# INLINE (<!>) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Functor.Plus(Plus(zero))
>>> let ValidationMonadT (Identity r) = view _Wrapped' (zero :: ValidatorMonadProfunctorT [String] Identity Int Int) 3
>>> r
Failure []
-}
instance (Monad f, Monoid err) => Plus (ValidatorMonadProfunctorT err f x) where
  zero = ValidatorMonadProfunctorT (const zero)
  {-# INLINE zero #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let ValidationMonadT (Identity r) = view _Wrapped' (empty :: ValidatorMonadProfunctorT [String] Identity Int Int) 3
>>> r
Failure []
-}
instance (Monad f, Monoid err) => Alternative (ValidatorMonadProfunctorT err f x) where
  empty = zero
  {-# INLINE empty #-}
  (<|>) = (<!>)
  {-# INLINE (<|>) #-}

instance (Monad f, Monoid err) => MonadPlus (ValidatorMonadProfunctorT err f x)

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Selective(Selective(select))
>>> let v = fmap Right (pure 1) :: ValidatorMonadProfunctorT [String] Identity Int (Either Int Int)
>>> let ValidationMonadT (Identity r) = view _Wrapped' (select v (pure (+1))) 3
>>> r
Success 1
-}
instance (Monad f) => Selective (ValidatorMonadProfunctorT err f x) where
  select = selectM
  {-# INLINE select #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Profunctor(Profunctor(dimap, lmap, rmap))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (dimap (+10) (*2) v) 3
>>> r
Success 28

>>> let ValidationMonadT (Identity r) = view _Wrapped' (lmap (+10) v) 3
>>> r
Success 14

>>> let ValidationMonadT (Identity r) = view _Wrapped' (rmap (*2) v) 3
>>> r
Success 8
-}
instance (Functor f) => Profunctor (ValidatorMonadProfunctorT err f) where
  dimap f g (ValidatorMonadProfunctorT h) = ValidatorMonadProfunctorT (fmap g . h . f)
  {-# INLINE dimap #-}
  lmap f (ValidatorMonadProfunctorT h) = ValidatorMonadProfunctorT (h . f)
  {-# INLINE lmap #-}
  rmap g (ValidatorMonadProfunctorT h) = ValidatorMonadProfunctorT (fmap g . h)
  {-# INLINE rmap #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Profunctor(Strong(first'))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (first' v) (3, "tag")
>>> r
Success (4,"tag")
-}
instance (Functor f) => Strong (ValidatorMonadProfunctorT err f) where
  first' (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (\(a, c) -> fmap (,c) (f a))
  {-# INLINE first' #-}
  second' (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (\(c, a) -> fmap (c,) (f a))
  {-# INLINE second' #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Profunctor(Choice(left'))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (left' v) (Left 3 :: Either Int String)
>>> r
Success (Left 4)

>>> let ValidationMonadT (Identity r) = view _Wrapped' (left' v) (Right "x" :: Either Int String)
>>> r
Success (Right "x")
-}
instance (Monad f) => Choice (ValidatorMonadProfunctorT err f) where
  left' (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (either (fmap Left . f) (pure . Right))
  {-# INLINE left' #-}
  right' (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (either (pure . Left) (fmap Right . f))
  {-# INLINE right' #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (traverse' v) [1, 2, 3]
>>> r
Success [2,3,4]
-}
instance (Monad f) => Traversing (ValidatorMonadProfunctorT err f) where
  traverse' (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (traverse f)
  {-# INLINE traverse' #-}
  wander t (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (t f)
  {-# INLINE wander #-}

{- |
>>> import Data.Profunctor.Sieve(Sieve(sieve))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = sieve v 3
>>> r
Success 4
-}
instance (Monad f) => Sieve (ValidatorMonadProfunctorT err f) (ValidationMonadT err f) where
  sieve (ValidatorMonadProfunctorT f) = f
  {-# INLINE sieve #-}

{- | Kleisli-like composition, short-circuiting on failure.

>>> import Control.Lens(view, _Wrapped')
>>> import Data.Semigroupoid(Semigroupoid(o))
>>> let v1 = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let v2 = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x * 2)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v2 `o` v1) 3
>>> r
Success 8
-}
instance (Monad f) => Semigroupoid (ValidatorMonadProfunctorT err f) where
  ValidatorMonadProfunctorT f `o` ValidatorMonadProfunctorT g = ValidatorMonadProfunctorT (g >=> f)
  {-# INLINE o #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Category(id, (.))
>>> import Prelude hiding (id, (.))
>>> let v1 = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let v2 = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x * 2)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v2 . v1) 3
>>> r
Success 8
-}
instance (Monad f) => Category (ValidatorMonadProfunctorT err f) where
  id = ValidatorMonadProfunctorT pure
  {-# INLINE id #-}
  ValidatorMonadProfunctorT f . ValidatorMonadProfunctorT g = ValidatorMonadProfunctorT (g >=> f)
  {-# INLINE (.) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Arrow(Arrow(arr))
>>> import Control.Category((.))
>>> import Prelude hiding ((.))
>>> let ValidationMonadT (Identity r) = view _Wrapped' (arr (+1) :: ValidatorMonadProfunctorT [String] Identity Int Int) 3
>>> r
Success 4
-}
instance (Monad f) => Arrow (ValidatorMonadProfunctorT err f) where
  arr f = ValidatorMonadProfunctorT (pure . f)
  {-# INLINE arr #-}
  first (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (\(a, c) -> fmap (,c) (f a))
  {-# INLINE first #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Arrow(ArrowApply(app))
>>> import Control.Category((.))
>>> import Prelude hiding ((.))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (app :: ValidatorMonadProfunctorT [String] Identity (ValidatorMonadProfunctorT [String] Identity Int Int, Int) Int) (v, 3)
>>> r
Success 4
-}
instance (Monad f) => ArrowApply (ValidatorMonadProfunctorT err f) where
  app = ValidatorMonadProfunctorT (\(ValidatorMonadProfunctorT f, x) -> f x)
  {-# INLINE app #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Arrow(ArrowChoice(left))
>>> import Control.Category((.))
>>> import Prelude hiding ((.))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (left v) (Left 3 :: Either Int String)
>>> r
Success (Left 4)
-}
instance (Monad f) => ArrowChoice (ValidatorMonadProfunctorT err f) where
  left (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (either (fmap Left . f) (pure . Right))
  {-# INLINE left #-}
  right (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (either (pure . Left) (fmap Right . f))
  {-# INLINE right #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Arrow(ArrowZero(zeroArrow))
>>> import Control.Category((.))
>>> import Prelude hiding ((.))
>>> let ValidationMonadT (Identity r) = view _Wrapped' (zeroArrow :: ValidatorMonadProfunctorT [String] Identity Int Int) 3
>>> r
Failure []
-}
instance (Monad f, Monoid err) => ArrowZero (ValidatorMonadProfunctorT err f) where
  zeroArrow = ValidatorMonadProfunctorT (const zero)
  {-# INLINE zeroArrow #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Functor.Alt(Alt((<!>)))
>>> import Control.Arrow(ArrowPlus((<+>)))
>>> import Control.Category((.))
>>> import Prelude hiding ((.))
>>> let v1 = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let v2 = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x * 2)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v1 <+> v2) 3
>>> r
Success 6
-}
instance (Monad f, Monoid err) => ArrowPlus (ValidatorMonadProfunctorT err f) where
  ValidatorMonadProfunctorT f <+> ValidatorMonadProfunctorT g = ValidatorMonadProfunctorT (\x -> f x <!> g x)
  {-# INLINE (<+>) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Data.Functor.Extend(Extend(extended))
>>> let v = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (extended (\_ -> 99) v) 3
>>> r
Success 99
-}
instance (Monad f) => Extend (ValidatorMonadProfunctorT err f x) where
  extended f w@(ValidatorMonadProfunctorT _) = ValidatorMonadProfunctorT (\_ -> pure (f w))
  {-# INLINE extended #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let v1 = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Success 1))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let v2 = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Success 2))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (v1 <> v2) 0
>>> r
Success 1
-}
instance (Applicative f, Semigroup err) => Semigroup (ValidatorMonadProfunctorT err f x a) where
  ValidatorMonadProfunctorT f <> ValidatorMonadProfunctorT g = ValidatorMonadProfunctorT (\x -> f x <> g x)
  {-# INLINE (<>) #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> let ValidationMonadT (Identity r) = view _Wrapped' (mempty :: ValidatorMonadProfunctorT [String] Identity Int Int) 0
>>> r
Failure []
-}
instance (Applicative f, Monoid err) => Monoid (ValidatorMonadProfunctorT err f x a) where
  mempty = ValidatorMonadProfunctorT (const mempty)
  {-# INLINE mempty #-}

instance (Monad f, MonadFail f) => MonadFail (ValidatorMonadProfunctorT err f x) where
  fail = ValidatorMonadProfunctorT . const . liftValidationMonadT . Prelude.fail
  {-# INLINE fail #-}

{- |
>>> import Control.Lens(view, _Wrapped')
>>> import Control.Monad.Error.Class(MonadError(throwError, catchError))
>>> let ValidationMonadT (Identity r) = view _Wrapped' (throwError ["oops"] :: ValidatorMonadProfunctorT [String] Identity Int Int) 3
>>> r
Failure ["oops"]

>>> let v = ValidatorMonadProfunctorT (\_ -> ValidationMonadT (Identity (Failure ["e1"]))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let h _ = ValidatorMonadProfunctorT (\x -> ValidationMonadT (Identity (Success (x * 2)))) :: ValidatorMonadProfunctorT [String] Identity Int Int
>>> let ValidationMonadT (Identity r) = view _Wrapped' (catchError v h) 3
>>> r
Success 6
-}
instance (Monad f) => MonadError err (ValidatorMonadProfunctorT err f x) where
  throwError e = ValidatorMonadProfunctorT (\_ -> throwError e)
  {-# INLINE throwError #-}
  catchError (ValidatorMonadProfunctorT f) h = ValidatorMonadProfunctorT (\x -> catchError (f x) (\e -> let ValidatorMonadProfunctorT g = h e in g x))
  {-# INLINE catchError #-}

instance (MonadIO f) => MonadIO (ValidatorMonadProfunctorT err f x) where
  liftIO = ValidatorMonadProfunctorT . const . liftValidationMonadT . liftIO
  {-# INLINE liftIO #-}

instance (MonadReader r f) => MonadReader r (ValidatorMonadProfunctorT err f x) where
  ask = ValidatorMonadProfunctorT (\_ -> liftValidationMonadT ask)
  {-# INLINE ask #-}
  local f (ValidatorMonadProfunctorT g) = ValidatorMonadProfunctorT (local f . g)
  {-# INLINE local #-}
  reader = ValidatorMonadProfunctorT . const . liftValidationMonadT . reader
  {-# INLINE reader #-}

instance (MonadWriter w f) => MonadWriter w (ValidatorMonadProfunctorT err f x) where
  writer = ValidatorMonadProfunctorT . const . liftValidationMonadT . writer
  {-# INLINE writer #-}
  tell = ValidatorMonadProfunctorT . const . liftValidationMonadT . tell
  {-# INLINE tell #-}
  listen (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (listen . f)
  {-# INLINE listen #-}
  pass (ValidatorMonadProfunctorT f) = ValidatorMonadProfunctorT (pass . f)
  {-# INLINE pass #-}

instance (MonadState s f) => MonadState s (ValidatorMonadProfunctorT err f x) where
  get = ValidatorMonadProfunctorT (\_ -> liftValidationMonadT get)
  {-# INLINE get #-}
  put = ValidatorMonadProfunctorT . const . liftValidationMonadT . put
  {-# INLINE put #-}
  state = ValidatorMonadProfunctorT . const . liftValidationMonadT . state
  {-# INLINE state #-}

instance (MonadCont f) => MonadCont (ValidatorMonadProfunctorT err f x) where
  callCC f = ValidatorMonadProfunctorT (\x -> callCC (\c -> let ValidatorMonadProfunctorT g = f (\a -> ValidatorMonadProfunctorT (\_ -> c a)) in g x))
  {-# INLINE callCC #-}

instance (MonadRWS r w s f) => MonadRWS r w s (ValidatorMonadProfunctorT err f x)

-- | Class for types that have a 'Getter' to a 'ValidatorMonadProfunctorT'.
class GetValidatorMonadProfunctorT s err f x a | s -> err f x a where
  getValidatorMonadProfunctorT :: Getter s (ValidatorMonadProfunctorT err f x a)

instance GetValidatorMonadProfunctorT (ValidatorMonadProfunctorT err f x a) err f x a where
  getValidatorMonadProfunctorT = id
  {-# INLINE getValidatorMonadProfunctorT #-}

-- | Class for types that have a 'Lens'' to a 'ValidatorMonadProfunctorT'.
class (GetValidatorMonadProfunctorT s err f x a) => HasValidatorMonadProfunctorT s err f x a | s -> err f x a where
  validatorMonadProfunctorT :: Lens' s (ValidatorMonadProfunctorT err f x a)

instance HasValidatorMonadProfunctorT (ValidatorMonadProfunctorT err f x a) err f x a where
  validatorMonadProfunctorT = id
  {-# INLINE validatorMonadProfunctorT #-}

-- | Class for types that have a 'Review' to a 'ValidatorMonadProfunctorT'.
class ReviewValidatorMonadProfunctorT s err f x a | s -> err f x a where
  reviewValidatorMonadProfunctorT :: Review s (ValidatorMonadProfunctorT err f x a)

instance ReviewValidatorMonadProfunctorT (ValidatorMonadProfunctorT err f x a) err f x a where
  reviewValidatorMonadProfunctorT = unto id
  {-# INLINE reviewValidatorMonadProfunctorT #-}

-- | Class for types that have a 'Prism'' to a 'ValidatorMonadProfunctorT'.
class (ReviewValidatorMonadProfunctorT s err f x a) => AsValidatorMonadProfunctorT s err f x a | s -> err f x a where
  _ValidatorMonadProfunctorT :: Prism' s (ValidatorMonadProfunctorT err f x a)

instance AsValidatorMonadProfunctorT (ValidatorMonadProfunctorT err f x a) err f x a where
  _ValidatorMonadProfunctorT = id
  {-# INLINE _ValidatorMonadProfunctorT #-}

-- =============================
-- Cross-type optics instances
-- =============================

-- Cross-type optics: Validator <-> ValidatorProfunctor

instance GetValidator (ValidatorProfunctor err x a) x err a where
  getValidator = iso (\(ValidatorProfunctor f) -> Validator f) (\(Validator f) -> ValidatorProfunctor f)
  {-# INLINE getValidator #-}

instance HasValidator (ValidatorProfunctor err x a) x err a where
  validator = iso (\(ValidatorProfunctor f) -> Validator f) (\(Validator f) -> ValidatorProfunctor f)
  {-# INLINE validator #-}

instance ReviewValidator (ValidatorProfunctor err x a) x err a where
  reviewValidator = unto (\(Validator f) -> ValidatorProfunctor f)
  {-# INLINE reviewValidator #-}

instance AsValidator (ValidatorProfunctor err x a) x err a where
  _Validator = iso (\(ValidatorProfunctor f) -> Validator f) (\(Validator f) -> ValidatorProfunctor f)
  {-# INLINE _Validator #-}

instance GetValidatorProfunctor (Validator x err a) err x a where
  getValidatorProfunctor = iso (\(Validator f) -> ValidatorProfunctor f) (\(ValidatorProfunctor f) -> Validator f)
  {-# INLINE getValidatorProfunctor #-}

instance HasValidatorProfunctor (Validator x err a) err x a where
  validatorProfunctor = iso (\(Validator f) -> ValidatorProfunctor f) (\(ValidatorProfunctor f) -> Validator f)
  {-# INLINE validatorProfunctor #-}

instance ReviewValidatorProfunctor (Validator x err a) err x a where
  reviewValidatorProfunctor = unto (\(ValidatorProfunctor f) -> Validator f)
  {-# INLINE reviewValidatorProfunctor #-}

instance AsValidatorProfunctor (Validator x err a) err x a where
  _ValidatorProfunctor = iso (\(Validator f) -> ValidatorProfunctor f) (\(ValidatorProfunctor f) -> Validator f)
  {-# INLINE _ValidatorProfunctor #-}

-- Cross-type optics: ValidatorMonadT <-> ValidatorMonadProfunctorT

instance GetValidatorMonadT (ValidatorMonadProfunctorT err f x a) x err f a where
  getValidatorMonadT = iso (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f) (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f)
  {-# INLINE getValidatorMonadT #-}

instance HasValidatorMonadT (ValidatorMonadProfunctorT err f x a) x err f a where
  validatorMonadT = iso (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f) (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f)
  {-# INLINE validatorMonadT #-}

instance ReviewValidatorMonadT (ValidatorMonadProfunctorT err f x a) x err f a where
  reviewValidatorMonadT = unto (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f)
  {-# INLINE reviewValidatorMonadT #-}

instance AsValidatorMonadT (ValidatorMonadProfunctorT err f x a) x err f a where
  _ValidatorMonadT = iso (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f) (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f)
  {-# INLINE _ValidatorMonadT #-}

instance GetValidatorMonadProfunctorT (ValidatorMonadT x err f a) err f x a where
  getValidatorMonadProfunctorT = iso (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f) (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f)
  {-# INLINE getValidatorMonadProfunctorT #-}

instance HasValidatorMonadProfunctorT (ValidatorMonadT x err f a) err f x a where
  validatorMonadProfunctorT = iso (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f) (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f)
  {-# INLINE validatorMonadProfunctorT #-}

instance ReviewValidatorMonadProfunctorT (ValidatorMonadT x err f a) err f x a where
  reviewValidatorMonadProfunctorT = unto (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f)
  {-# INLINE reviewValidatorMonadProfunctorT #-}

instance AsValidatorMonadProfunctorT (ValidatorMonadT x err f a) err f x a where
  _ValidatorMonadProfunctorT = iso (\(ValidatorMonadT f) -> ValidatorMonadProfunctorT f) (\(ValidatorMonadProfunctorT f) -> ValidatorMonadT f)
  {-# INLINE _ValidatorMonadProfunctorT #-}

-- Cross-type optics: Validator <-> ValidatorMonadT (f ~ Identity)

instance GetValidator (ValidatorMonadT x err Identity a) x err a where
  getValidator = iso (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f)) (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f))
  {-# INLINE getValidator #-}

instance HasValidator (ValidatorMonadT x err Identity a) x err a where
  validator = iso (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f)) (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f))
  {-# INLINE validator #-}

instance ReviewValidator (ValidatorMonadT x err Identity a) x err a where
  reviewValidator = unto (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f))
  {-# INLINE reviewValidator #-}

instance AsValidator (ValidatorMonadT x err Identity a) x err a where
  _Validator = iso (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f)) (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f))
  {-# INLINE _Validator #-}

instance GetValidatorMonadT (Validator x err a) x err Identity a where
  getValidatorMonadT = iso (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f)) (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE getValidatorMonadT #-}

instance HasValidatorMonadT (Validator x err a) x err Identity a where
  validatorMonadT = iso (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f)) (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE validatorMonadT #-}

instance ReviewValidatorMonadT (Validator x err a) x err Identity a where
  reviewValidatorMonadT = unto (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE reviewValidatorMonadT #-}

instance AsValidatorMonadT (Validator x err a) x err Identity a where
  _ValidatorMonadT = iso (\(Validator f) -> ValidatorMonadT (ValidationMonadT . Identity . f)) (\(ValidatorMonadT f) -> Validator (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE _ValidatorMonadT #-}

-- Cross-type optics: ValidatorProfunctor <-> ValidatorMonadProfunctorT (f ~ Identity)

instance GetValidatorProfunctor (ValidatorMonadProfunctorT err Identity x a) err x a where
  getValidatorProfunctor = iso (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f)) (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f))
  {-# INLINE getValidatorProfunctor #-}

instance HasValidatorProfunctor (ValidatorMonadProfunctorT err Identity x a) err x a where
  validatorProfunctor = iso (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f)) (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f))
  {-# INLINE validatorProfunctor #-}

instance ReviewValidatorProfunctor (ValidatorMonadProfunctorT err Identity x a) err x a where
  reviewValidatorProfunctor = unto (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f))
  {-# INLINE reviewValidatorProfunctor #-}

instance AsValidatorProfunctor (ValidatorMonadProfunctorT err Identity x a) err x a where
  _ValidatorProfunctor = iso (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f)) (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f))
  {-# INLINE _ValidatorProfunctor #-}

instance GetValidatorMonadProfunctorT (ValidatorProfunctor err x a) err Identity x a where
  getValidatorMonadProfunctorT = iso (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f)) (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE getValidatorMonadProfunctorT #-}

instance HasValidatorMonadProfunctorT (ValidatorProfunctor err x a) err Identity x a where
  validatorMonadProfunctorT = iso (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f)) (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE validatorMonadProfunctorT #-}

instance ReviewValidatorMonadProfunctorT (ValidatorProfunctor err x a) err Identity x a where
  reviewValidatorMonadProfunctorT = unto (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE reviewValidatorMonadProfunctorT #-}

instance AsValidatorMonadProfunctorT (ValidatorProfunctor err x a) err Identity x a where
  _ValidatorMonadProfunctorT = iso (\(ValidatorProfunctor f) -> ValidatorMonadProfunctorT (ValidationMonadT . Identity . f)) (\(ValidatorMonadProfunctorT f) -> ValidatorProfunctor (runIdentity . (\(ValidationMonadT m) -> m) . f))
  {-# INLINE _ValidatorMonadProfunctorT #-}
