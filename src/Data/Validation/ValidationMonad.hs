{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- \$setup
-- >>> import Data.Functor.Identity(Identity(..))
-- >>> import Data.Validation.Validation(Validation(..))
-- >>> import Data.Validation.ValidationMonad
-- >>> import Control.Lens(view, _Wrapped', review, (#), (^?), from)
-- >>> import Data.Functor.Alt(Alt((<!>)))
-- >>> import Data.Functor.Apply(Apply((<.>)))
-- >>> import Data.Functor.Extend(Extend(extended))
-- >>> import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
-- >>> import Control.Monad.Error.Class(MonadError(throwError, catchError))
-- >>> import Control.Monad.Trans.Class(MonadTrans(lift))
-- >>> import Control.DeepSeq(rnf)
-- >>> import Data.Functor.Plus(Plus(zero))
-- >>> :set -XNoMonomorphismRestriction -w

{- | A monad transformer wrapping @m (Validation err a)@ with short-circuiting
'Applicative' and 'Monad' instances, unlike 'Validation' which accumulates errors.
-}
module Data.Validation.ValidationMonad (
  ValidationMonadT (..),
  ValidationMonad,
  liftValidationMonadT,

  -- * Isomorphisms
  validationMonad,

  -- * Optics

  -- ** Classy lenses
  GetValidationMonadT (..),
  HasValidationMonadT (..),

  -- ** Classy prisms
  ReviewValidationMonadT (..),
  AsValidationMonadT (..),
) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.DeepSeq (NFData (rnf))
import Control.Lens (Getter, Lens', Prism', Review, Rewrapped, Wrapped (_Wrapped', type Unwrapped), from, prism', unto)
import Control.Lens.Iso (Iso, iso)
import Control.Monad (MonadPlus, ap)
import Control.Monad.Cont.Class (MonadCont (callCC))
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader (ask, local, reader))
import Control.Monad.State.Class (MonadState (get, put, state))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer.Class (MonadWriter (listen, pass, tell, writer))
import Control.Selective (Selective (select), selectM)
import qualified Data.Either as Either
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Bind.Trans (BindTrans (liftB))
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowList, liftShowsPrec))
import Data.Functor.Extend (Extend (extended))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Plus (Plus (zero))
import Data.Validation.Validation (AsValidation (..), GetValidation (..), HasValidation (..), ReviewValidation (..), Validation (..), foldValidation)
import GHC.Generics (Generic)

{- | A monad transformer wrapping @m (Validation err a)@.

>>> ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 1))

>>> ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Failure "err"))
-}
newtype ValidationMonadT err m a = ValidationMonadT (m (Validation err a))
  deriving (Generic)

-- | Type alias for @ValidationMonadT err Identity a@.
type ValidationMonad err a = ValidationMonadT err Identity a

{- |
>>> ValidationMonadT (Identity (Success 1)) == (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
True

>>> ValidationMonadT (Identity (Success 1)) == (ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int)
False
-}
deriving instance (Eq (m (Validation err a))) => Eq (ValidationMonadT err m a)

{- |
>>> compare (ValidationMonadT (Identity (Failure "a"))) (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
LT
-}
deriving instance (Ord (m (Validation err a))) => Ord (ValidationMonadT err m a)

{- |
>>> show (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
"ValidationMonadT (Identity (Success 1))"
-}
deriving instance (Show (m (Validation err a))) => Show (ValidationMonadT err m a)

{- |
>>> import Control.Lens(view, _Wrapped')
>>> view _Wrapped' (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
Identity (Success 1)
-}
instance Wrapped (ValidationMonadT err m a) where
  type Unwrapped (ValidationMonadT err m a) = m (Validation err a)
  _Wrapped' = iso (\(ValidationMonadT m) -> m) ValidationMonadT
  {-# INLINE _Wrapped' #-}

instance Rewrapped (ValidationMonadT err m a) (ValidationMonadT err' m' b)

{- |
>>> liftEq (==) (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int) (ValidationMonadT (Identity (Success 1)))
True

>>> liftEq (==) (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int) (ValidationMonadT (Identity (Success 2)))
False
-}
instance (Eq1 m, Eq err) => Eq1 (ValidationMonadT err m) where
  liftEq f (ValidationMonadT ma) (ValidationMonadT mb) = liftEq (liftEq f) ma mb
  {-# INLINE liftEq #-}

{- |
>>> liftCompare compare (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int) (ValidationMonadT (Identity (Success 2)))
LT
-}
instance (Ord1 m, Ord err) => Ord1 (ValidationMonadT err m) where
  liftCompare f (ValidationMonadT ma) (ValidationMonadT mb) = liftCompare (liftCompare f) ma mb
  {-# INLINE liftCompare #-}

instance (Show1 m, Show err) => Show1 (ValidationMonadT err m) where
  liftShowsPrec sp sl d (ValidationMonadT m) =
    showParen (d > 10) $
      showString "ValidationMonadT " . liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl) 11 m
  {-# INLINE liftShowsPrec #-}

{- | Lift a value from the base functor into 'ValidationMonadT'.

>>> liftValidationMonadT (Identity 1) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 1))
-}
liftValidationMonadT :: (Functor m) => m a -> ValidationMonadT err m a
liftValidationMonadT = ValidationMonadT . fmap Success
{-# INLINE liftValidationMonadT #-}

{- |
>>> fmap (+1) (ValidationMonadT (Identity (Success 2)) :: ValidationMonadT String Identity Int)
ValidationMonadT (Identity (Success 3))

>>> fmap (+1) (ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int)
ValidationMonadT (Identity (Failure "err"))
-}
instance (Functor m) => Functor (ValidationMonadT err m) where
  fmap f (ValidationMonadT m) = ValidationMonadT (fmap (fmap f) m)
  {-# INLINE fmap #-}

{- | Short-circuiting: stops at the first 'Failure'.

>>> (ValidationMonadT (Identity (Success (+1))) :: ValidationMonadT String Identity (Int -> Int)) <.> ValidationMonadT (Identity (Success 2))
ValidationMonadT (Identity (Success 3))

>>> (ValidationMonadT (Identity (Failure "e1")) :: ValidationMonadT String Identity (Int -> Int)) <.> ValidationMonadT (Identity (Success 2))
ValidationMonadT (Identity (Failure "e1"))
-}
instance (Monad m) => Apply (ValidationMonadT err m) where
  (<.>) = ap
  {-# INLINE (<.>) #-}

{- | Short-circuiting: unlike 'Validation', does /not/ accumulate errors.

>>> pure 1 :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 1))

>>> (ValidationMonadT (Identity (Failure "e1")) :: ValidationMonadT String Identity (Int -> Int)) <*> (ValidationMonadT (Identity (Failure "e2")) :: ValidationMonadT String Identity Int)
ValidationMonadT (Identity (Failure "e1"))
-}
instance (Monad m) => Applicative (ValidationMonadT err m) where
  pure = ValidationMonadT . pure . Success
  {-# INLINE pure #-}
  ValidationMonadT mf <*> ValidationMonadT ma = ValidationMonadT $ do
    vf <- mf
    case vf of
      Failure e -> pure (Failure e)
      Success f -> fmap (fmap f) ma
  {-# INLINE (<*>) #-}

instance (Monad m) => Bind (ValidationMonadT err m) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

{- | Short-circuiting on the first 'Failure'.

>>> ValidationMonadT (Identity (Success 2)) >>= (\x -> ValidationMonadT (Identity (Success (x + 1)))) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 3))

>>> (ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int) >>= (\x -> ValidationMonadT (Identity (Success (x + 1))))
ValidationMonadT (Identity (Failure "err"))
-}
instance (Monad m) => Monad (ValidationMonadT err m) where
  ValidationMonadT m >>= k = ValidationMonadT $ do
    va <- m
    case va of
      Failure e -> pure (Failure e)
      Success a -> let ValidationMonadT n = k a in n
  {-# INLINE (>>=) #-}

instance (Monad m, MonadFail m) => MonadFail (ValidationMonadT err m) where
  fail = liftValidationMonadT . fail
  {-# INLINE fail #-}

{- | First 'Success' wins; two 'Failure's accumulate errors.

>>> (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT [String] Identity Int) <!> ValidationMonadT (Identity (Success 2))
ValidationMonadT (Identity (Success 1))

>>> (ValidationMonadT (Identity (Failure ["e1"])) :: ValidationMonadT [String] Identity Int) <!> ValidationMonadT (Identity (Success 2))
ValidationMonadT (Identity (Success 2))

>>> (ValidationMonadT (Identity (Failure ["e1"])) :: ValidationMonadT [String] Identity Int) <!> ValidationMonadT (Identity (Failure ["e2"]))
ValidationMonadT (Identity (Failure ["e1","e2"]))
-}
instance (Monad m, Semigroup err) => Alt (ValidationMonadT err m) where
  ValidationMonadT ma <!> ValidationMonadT mb = ValidationMonadT $ do
    va <- ma
    case va of
      Success a -> pure (Success a)
      Failure e1 -> fmap (foldValidation (Failure . (e1 <>)) Success) mb
  {-# INLINE (<!>) #-}

{- |
>>> zero :: ValidationMonadT [String] Identity Int
ValidationMonadT (Identity (Failure []))
-}
instance (Monad m, Monoid err) => Plus (ValidationMonadT err m) where
  zero = ValidationMonadT (pure (Failure mempty))
  {-# INLINE zero #-}

instance (Monad m, Monoid err) => Alternative (ValidationMonadT err m) where
  empty = zero
  {-# INLINE empty #-}
  (<|>) = (<!>)
  {-# INLINE (<|>) #-}

instance (Monad m, Monoid err) => MonadPlus (ValidationMonadT err m)

instance (Monad m) => Selective (ValidationMonadT err m) where
  select = selectM
  {-# INLINE select #-}

{- |
>>> foldr (:) [] (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
[1]

>>> foldr (:) [] (ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int)
[]
-}
instance (Foldable m) => Foldable (ValidationMonadT err m) where
  foldr f z (ValidationMonadT m) = foldr (flip (foldr f)) z m
  {-# INLINE foldr #-}

{- |
>>> traverse (\x -> [x, x+1]) (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
[ValidationMonadT (Identity (Success 1)),ValidationMonadT (Identity (Success 2))]

>>> traverse (\x -> [x, x+1]) (ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int)
[ValidationMonadT (Identity (Failure "err"))]
-}
instance (Traversable m) => Traversable (ValidationMonadT err m) where
  traverse f (ValidationMonadT m) = ValidationMonadT <$> traverse (traverse f) m
  {-# INLINE traverse #-}

{- |
>>> extended (\_ -> 42) (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 42))

>>> extended (\_ -> 42) (ValidationMonadT (Identity (Failure "err")) :: ValidationMonadT String Identity Int) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Failure "err"))
-}
instance (Functor m) => Extend (ValidationMonadT err m) where
  extended f w@(ValidationMonadT m) = ValidationMonadT (fmap (foldValidation Failure (const (Success (f w)))) m)
  {-# INLINE extended #-}

{- |
>>> (ValidationMonadT (Identity (Failure ["e1"])) :: ValidationMonadT [String] Identity Int) <> ValidationMonadT (Identity (Failure ["e2"]))
ValidationMonadT (Identity (Failure ["e1","e2"]))

>>> (ValidationMonadT (Identity (Failure ["e1"])) :: ValidationMonadT [String] Identity Int) <> ValidationMonadT (Identity (Success 2))
ValidationMonadT (Identity (Success 2))
-}
instance (Applicative m, Semigroup e) => Semigroup (ValidationMonadT e m a) where
  ValidationMonadT ma <> ValidationMonadT mb = ValidationMonadT (liftA2 (<>) ma mb)
  {-# INLINE (<>) #-}

{- |
>>> mempty :: ValidationMonadT [String] Identity Int
ValidationMonadT (Identity (Failure []))
-}
instance (Applicative m, Monoid e) => Monoid (ValidationMonadT e m a) where
  mempty = ValidationMonadT (pure mempty)
  {-# INLINE mempty #-}

{- |
>>> rnf (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
()
-}
instance (NFData (m (Validation err a))) => NFData (ValidationMonadT err m a) where
  rnf (ValidationMonadT m) = rnf m
  {-# INLINE rnf #-}

{- |
>>> lift (Identity 1) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 1))
-}
instance MonadTrans (ValidationMonadT err) where
  lift = liftValidationMonadT
  {-# INLINE lift #-}

instance BindTrans (ValidationMonadT err) where
  liftB = liftValidationMonadT
  {-# INLINE liftB #-}

{- |
>>> throwError "err" :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Failure "err"))

>>> catchError (throwError "err" :: ValidationMonadT String Identity Int) (\e -> pure (length e))
ValidationMonadT (Identity (Success 3))
-}
instance (Monad m) => MonadError err (ValidationMonadT err m) where
  throwError = ValidationMonadT . pure . Failure
  {-# INLINE throwError #-}
  catchError (ValidationMonadT m) h = ValidationMonadT $ do
    va <- m
    case va of
      Failure e -> let ValidationMonadT n = h e in n
      Success a -> pure (Success a)
  {-# INLINE catchError #-}

instance (MonadIO m) => MonadIO (ValidationMonadT err m) where
  liftIO = liftValidationMonadT . liftIO
  {-# INLINE liftIO #-}

instance (MonadReader r m) => MonadReader r (ValidationMonadT err m) where
  ask = liftValidationMonadT ask
  {-# INLINE ask #-}
  local f (ValidationMonadT m) = ValidationMonadT (local f m)
  {-# INLINE local #-}
  reader = liftValidationMonadT . reader
  {-# INLINE reader #-}

instance (MonadWriter w m) => MonadWriter w (ValidationMonadT err m) where
  writer = liftValidationMonadT . writer
  {-# INLINE writer #-}
  tell = liftValidationMonadT . tell
  {-# INLINE tell #-}
  listen (ValidationMonadT m) = ValidationMonadT $ do
    (va, w) <- listen m
    pure (fmap (,w) va)
  {-# INLINE listen #-}
  pass (ValidationMonadT m) = ValidationMonadT $ pass $ do
    va <- m
    pure $ case va of
      Failure e -> (Failure e, id)
      Success (a, f) -> (Success a, f)
  {-# INLINE pass #-}

instance (MonadState s m) => MonadState s (ValidationMonadT err m) where
  get = liftValidationMonadT get
  {-# INLINE get #-}
  put = liftValidationMonadT . put
  {-# INLINE put #-}
  state = liftValidationMonadT . state
  {-# INLINE state #-}

instance (MonadCont m) => MonadCont (ValidationMonadT err m) where
  callCC f = ValidationMonadT $ callCC $ \c ->
    let ValidationMonadT m = f (ValidationMonadT . c . Success) in m
  {-# INLINE callCC #-}

instance (MonadRWS r w s m) => MonadRWS r w s (ValidationMonadT err m)

{- | Class for types that have a 'Getter' to a 'ValidationMonadT'.

>>> import Control.Lens(view)
>>> view getValidationMonadT (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
ValidationMonadT (Identity (Success 1))
-}
class GetValidationMonadT s err m a | s -> err m a where
  getValidationMonadT :: Getter s (ValidationMonadT err m a)

instance GetValidationMonadT (ValidationMonadT err m a) err m a where
  getValidationMonadT = id
  {-# INLINE getValidationMonadT #-}

{- | Class for types that have a 'Lens'' to a 'ValidationMonadT'.

>>> import Control.Lens(view)
>>> view validationMonadT (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
ValidationMonadT (Identity (Success 1))
-}
class (GetValidationMonadT s err m a) => HasValidationMonadT s err m a | s -> err m a where
  validationMonadT :: Lens' s (ValidationMonadT err m a)

instance HasValidationMonadT (ValidationMonadT err m a) err m a where
  validationMonadT = id
  {-# INLINE validationMonadT #-}

{- | Class for types that have a 'Review' to a 'ValidationMonadT'.

>>> import Control.Lens(review)
>>> review reviewValidationMonadT (ValidationMonadT (Identity (Success 1))) :: ValidationMonadT String Identity Int
ValidationMonadT (Identity (Success 1))
-}
class ReviewValidationMonadT s err m a | s -> err m a where
  reviewValidationMonadT :: Review s (ValidationMonadT err m a)

instance ReviewValidationMonadT (ValidationMonadT err m a) err m a where
  reviewValidationMonadT = unto id
  {-# INLINE reviewValidationMonadT #-}

-- | Class for types that have a 'Prism'' to a 'ValidationMonadT'.
class (ReviewValidationMonadT s err m a) => AsValidationMonadT s err m a | s -> err m a where
  _ValidationMonadT :: Prism' s (ValidationMonadT err m a)

instance AsValidationMonadT (ValidationMonadT err m a) err m a where
  _ValidationMonadT = id
  {-# INLINE _ValidationMonadT #-}

{- | Isomorphism between @Validation err a@ and @ValidationMonadT err Identity a@.

>>> import Control.Lens(view, from)
>>> view validationMonad (Success 1 :: Validation String Int)
ValidationMonadT (Identity (Success 1))

>>> view (from validationMonad) (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
Success 1
-}
validationMonad :: Iso (Validation err a) (Validation err' a') (ValidationMonad err a) (ValidationMonad err' a')
validationMonad = iso (ValidationMonadT . pure) (\(ValidationMonadT (Identity v)) -> v)
{-# INLINE validationMonad #-}

{- |
>>> import Control.Lens(view)
>>> view getValidationMonadT (Success 1 :: Validation String Int)
ValidationMonadT (Identity (Success 1))
-}
instance GetValidationMonadT (Validation err a) err Identity a where
  getValidationMonadT = validationMonad
  {-# INLINE getValidationMonadT #-}

{- |
>>> import Control.Lens(view)
>>> view validationMonadT (Success 1 :: Validation String Int)
ValidationMonadT (Identity (Success 1))
-}
instance HasValidationMonadT (Validation err a) err Identity a where
  validationMonadT = validationMonad
  {-# INLINE validationMonadT #-}

{- |
>>> import Control.Lens(review)
>>> review reviewValidationMonadT (ValidationMonadT (Identity (Success 1))) :: Validation String Int
Success 1
-}
instance ReviewValidationMonadT (Validation err a) err Identity a where
  reviewValidationMonadT = unto (\(ValidationMonadT (Identity v)) -> v)
  {-# INLINE reviewValidationMonadT #-}

{- |
>>> import Control.Lens((^?))
>>> (Success 1 :: Validation String Int) ^? _ValidationMonadT
Just (ValidationMonadT (Identity (Success 1)))
-}
instance AsValidationMonadT (Validation err a) err Identity a where
  _ValidationMonadT =
    prism'
      (\(ValidationMonadT (Identity v)) -> v)
      (Just . ValidationMonadT . pure)
  {-# INLINE _ValidationMonadT #-}

{- |
>>> import Control.Lens(view)
>>> view getValidation (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
Success 1
-}
instance GetValidation (ValidationMonad err a) err a where
  getValidation = from validationMonad
  {-# INLINE getValidation #-}

{- |
>>> import Control.Lens(view)
>>> view validation (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int)
Success 1
-}
instance HasValidation (ValidationMonad err a) err a where
  validation = from validationMonad
  {-# INLINE validation #-}

instance ReviewValidation (ValidationMonad err a) err a where
  reviewValidation = unto (ValidationMonadT . Identity)
  {-# INLINE reviewValidation #-}

{- |
>>> import Control.Lens((^?))
>>> (ValidationMonadT (Identity (Success 1)) :: ValidationMonadT String Identity Int) ^? _Validation
Just (Success 1)
-}
instance AsValidation (ValidationMonad err a) err a where
  _Validation = from validationMonad
  {-# INLINE _Validation #-}

{- |
>>> import Control.Lens(view)
>>> view getValidationMonadT (Left "err" :: Either String Int)
ValidationMonadT (Identity (Failure "err"))

>>> view getValidationMonadT (Right 1 :: Either String Int)
ValidationMonadT (Identity (Success 1))
-}
instance GetValidationMonadT (Either err a) err Identity a where
  getValidationMonadT = iso (ValidationMonadT . Identity . Either.either Failure Success) (\(ValidationMonadT (Identity v)) -> foldValidation Left Right v)
  {-# INLINE getValidationMonadT #-}

{- |
>>> import Control.Lens(view, set)
>>> view validationMonadT (Left "err" :: Either String Int)
ValidationMonadT (Identity (Failure "err"))

>>> set validationMonadT (ValidationMonadT (Identity (Success 2))) (Left "err" :: Either String Int)
Right 2
-}
instance HasValidationMonadT (Either err a) err Identity a where
  validationMonadT = iso (ValidationMonadT . Identity . Either.either Failure Success) (\(ValidationMonadT (Identity v)) -> foldValidation Left Right v)
  {-# INLINE validationMonadT #-}

{- |
>>> import Control.Lens(review)
>>> review reviewValidationMonadT (ValidationMonadT (Identity (Success 1))) :: Either String Int
Right 1

>>> review reviewValidationMonadT (ValidationMonadT (Identity (Failure "err"))) :: Either String Int
Left "err"
-}
instance ReviewValidationMonadT (Either err a) err Identity a where
  reviewValidationMonadT = unto (\(ValidationMonadT (Identity v)) -> foldValidation Left Right v)
  {-# INLINE reviewValidationMonadT #-}

{- |
>>> import Control.Lens((^?))
>>> (Left "err" :: Either String Int) ^? _ValidationMonadT
Just (ValidationMonadT (Identity (Failure "err")))

>>> (Right 1 :: Either String Int) ^? _ValidationMonadT
Just (ValidationMonadT (Identity (Success 1)))
-}
instance AsValidationMonadT (Either err a) err Identity a where
  _ValidationMonadT = iso (ValidationMonadT . Identity . Either.either Failure Success) (\(ValidationMonadT (Identity v)) -> foldValidation Left Right v)
  {-# INLINE _ValidationMonadT #-}
