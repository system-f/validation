{-# LANGUAGE DeriveDataTypeable #-}

-- | Data types similar to @Data.Either@ that are explicit about failure and success.
module Data.Validation
(
  AccValidation
, Validation
, ValidationT(..)
, Validate(..)
, FoldValidate(..)
, fromFailure
, fromSuccess
, getFailure
, getSuccess
, getFailureOr
, getSuccessOr
) where

import Data.Pointed
import Data.Copointed
import Control.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Typeable
import Data.Data

-- | A value of the type @err@ or @a@, however, the @Applicative@ instance
-- accumulates values. This is witnessed by the @Semigroup@ context on the instance.
-- /Note that there is no @Monad@ such that @ap = (<*>)./
--
-- * @success (+1) <*> success 7 == AccSuccess 8@
--
-- * @failure ["f1"] <*> success 7 == AccFailure ["f1"]@
--
-- * @success (+1) <*> failure ["f2"] == AccFailure ["f2"]@
--
-- * @failure ["f1"] <*> failure ["f2"] == AccFailure ["f1","f2"]@
data AccValidation err a =
  AccFailure err
  | AccSuccess a
  deriving (Eq, Ord, Show, Data, Typeable)

instance Functor (AccValidation err) where
  fmap _ (AccFailure e) = AccFailure e
  fmap f (AccSuccess a) = AccSuccess (f a)

instance Pointed (AccValidation err) where
  point = AccSuccess

instance Semigroup err => Applicative (AccValidation err) where
  pure = point
  AccFailure e1 <*> AccFailure e2 = AccFailure (e1 <> e2)
  AccFailure e1 <*> AccSuccess _  = AccFailure e1
  AccSuccess _  <*> AccFailure e2 = AccFailure e2
  AccSuccess f  <*> AccSuccess a  = AccSuccess (f a)

-- | A value of the type @err@ or @a@ and isomorphic to @Data.Either@.
data Validation err a =
  Failure err
  | Success a
  deriving (Eq, Ord, Show, Data, Typeable)

instance Functor (Validation err) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Pointed (Validation err) where
  point = Success

instance Applicative (Validation err) where
  pure = point
  Failure e1 <*> Failure _  = Failure e1
  Failure e1 <*> Success _  = Failure e1
  Success _  <*> Failure e2 = Failure e2
  Success f  <*> Success a  = Success (f a)

instance Monad (Validation err) where
  return = point
  Failure e >>= _ = Failure e
  Success a >>= f = f a

-- | The transformer version of @Validation@.
data ValidationT m err a =
  ValidationT {
    runValidationT :: m (Validation err a)
  }

instance Functor m => Functor (ValidationT m err) where
  fmap f (ValidationT k) =
    ValidationT (fmap (fmap f) k)

instance Pointed m => Pointed (ValidationT m err) where
  point = ValidationT . point . point

instance Applicative m => Applicative (ValidationT m err) where
  pure = ValidationT . pure . point
  ValidationT f <*> ValidationT a = ValidationT (liftA2 (<*>) f a)

instance Monad m => Monad (ValidationT m err) where
  return = ValidationT . return . point
  ValidationT v >>= f = ValidationT (v >>= \w -> case w
                                                 of Failure e -> return (Failure e)
                                                    Success a -> runValidationT (f a))

-- | Construction for validation values.
class Validate v where
  -- | Construct a success validation value.
  success ::
    a
    -> v err a
  -- | Construct a failure validation value.
  failure ::
    err
    -> v err a

instance Validate AccValidation where
  success =
    point
  failure =
    AccFailure

instance Validate Validation where
  success =
    point
  failure =
    Failure

instance Pointed m => Validate (ValidationT m) where
  success =
    ValidationT . point . success
  failure =
    ValidationT . point . failure

-- | Reduction for validation values.
class FoldValidate v where
  -- | Reduce a validation value /(catamorphism)/.
  foldValidate ::
    (err -> x) -- ^ The function to run if a failure value.
    -> (a -> x) -- ^ The function to run if a success value.
    -> v err a -- ^ The validation value.
    -> x

instance FoldValidate AccValidation where
  foldValidate f _ (AccFailure err) =
    f err
  foldValidate _ g (AccSuccess a) =
    g a

instance FoldValidate Validation where
  foldValidate f _ (Failure err) =
    f err
  foldValidate _ g (Success a) =
    g a

instance Copointed m => FoldValidate (ValidationT m) where
  foldValidate f z (ValidationT v) =
    foldValidate f z (copoint v)

-- | Returns the failure value or runs the given function on the success value to get a failure value.
fromFailure ::
  FoldValidate v =>
  (a -> err) -- ^ The function to run on the success value.
  -> v err a
  -> err
fromFailure =
  foldValidate id

-- | Returns the success value or runs the given function on the failure value to get a success value.
fromSuccess ::
  FoldValidate v =>
  (err -> a) -- ^ The function to run on the failure value.
  -> v err a
  -> a
fromSuccess =
  flip foldValidate id

-- | Returns the possible failure value if there is one.
getFailure ::
  FoldValidate v =>
  v err a
  -> Maybe err
getFailure =
  foldValidate Just (const Nothing)

-- | Returns the possible success value if there is one.
getSuccess ::
  FoldValidate v =>
  v err a
  -> Maybe a
getSuccess =
  foldValidate (const Nothing) Just

-- | Returns the failure value if there is one, otherwise returns the given default.
getFailureOr ::
  FoldValidate v =>
  err -- ^ The default to return if there is no failure value to return.
  -> v err a
  -> err
getFailureOr e =
  fromMaybe e . getFailure

-- | Returns the success value if there is one, otherwise returns the given default.
getSuccessOr ::
  FoldValidate v =>
  a -- ^ The default to return if there is no success value to return.
  -> v err a
  -> a
getSuccessOr a =
  fromMaybe a . getSuccess

