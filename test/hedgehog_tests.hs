{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (liftA3)
import Control.Category (id, (.))
import Control.Lens (Wrapped (_Wrapped'), from, review, view, (#), (^.), (^?))
import Control.Monad (join, unless)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Swap (swap)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Semigroupoid (Semigroupoid (o))
import Data.Validation
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Prelude hiding (either, id, (.))
import qualified Prelude

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  result <-
    checkParallel $
      Group
        "Validation"
        [ ("prop_semigroup_assoc", prop_semigroup_assoc),
          ("prop_monoid_assoc", prop_monoid_assoc),
          ("prop_monoid_left_id", prop_monoid_left_id),
          ("prop_monoid_right_id", prop_monoid_right_id),
          ("prop_functor_id", prop_functor_id),
          ("prop_functor_compose", prop_functor_compose),
          ("prop_applicative_id", prop_applicative_id),
          ("prop_applicative_homomorphism", prop_applicative_homomorphism),
          ("prop_apply_compose", prop_apply_compose),
          ("prop_alt_assoc", prop_alt_assoc),
          ("prop_alt_left_catch", prop_alt_left_catch),
          ("prop_bifunctor_id", prop_bifunctor_id),
          ("prop_bifunctor_compose", prop_bifunctor_compose),
          ("prop_foldValidation_failure", prop_foldValidation_failure),
          ("prop_foldValidation_success", prop_foldValidation_success),
          ("prop_either_roundtrip", prop_either_roundtrip),
          ("prop_either_roundtrip_inv", prop_either_roundtrip_inv),
          ("prop_codiagonal_roundtrip", prop_codiagonal_roundtrip),
          ("prop_failure_prism_review_preview", prop_failure_prism_review_preview),
          ("prop_success_prism_review_preview", prop_success_prism_review_preview),
          ("prop_failure_prism_miss", prop_failure_prism_miss),
          ("prop_success_prism_miss", prop_success_prism_miss),
          ("prop_poly_failure_prism", prop_poly_failure_prism),
          ("prop_poly_success_prism", prop_poly_success_prism),
          ("prop_swap_failure", prop_swap_failure),
          ("prop_swap_success", prop_swap_success),
          ("prop_swap_involution", prop_swap_involution),
          ("prop_validator_functor_id", prop_validator_functor_id),
          ("prop_validator_category_left_id", prop_validator_category_left_id),
          ("prop_validator_category_right_id", prop_validator_category_right_id),
          ("prop_validator_category_assoc", prop_validator_category_assoc),
          ("prop_validator_semigroupoid_assoc", prop_validator_semigroupoid_assoc),
          ("prop_validator_apply_accumulates", prop_validator_apply_accumulates),
          ("prop_validator_alt_accumulates", prop_validator_alt_accumulates),
          ("prop_validator_alt_left_success", prop_validator_alt_left_success),
          ("prop_either_reviewFailure", prop_either_reviewFailure),
          ("prop_either_asFailure_hit", prop_either_asFailure_hit),
          ("prop_either_asFailure_miss", prop_either_asFailure_miss),
          ("prop_either_reviewSuccess", prop_either_reviewSuccess),
          ("prop_either_asSuccess_hit", prop_either_asSuccess_hit),
          ("prop_either_asSuccess_miss", prop_either_asSuccess_miss),
          ("prop_either_failure_roundtrip", prop_either_failure_roundtrip),
          ("prop_either_success_roundtrip", prop_either_success_roundtrip)
        ]

  unless result exitFailure

-- Generators

genValidation :: Gen e -> Gen a -> Gen (Validation e a)
genValidation e a = Gen.choice [fmap Failure e, fmap Success a]

genInt :: Gen Int
genInt = Gen.int (Range.linear (-100) 100)

genString :: Gen String
genString = Gen.string (Range.linear 0 50) Gen.unicode

genStrings :: Gen [String]
genStrings = Gen.list (Range.linear 1 10) genString

testGen :: Gen (Validation [String] Int)
testGen = genValidation genStrings genInt

runV :: Validator' e x a -> x -> Validation e a
runV = view _Wrapped'

validators :: [Validator' [String] Int Int]
validators =
  [ Validator (Success . (+ 1)),
    Validator (Success . (* 2)),
    Validator (Success . negate),
    Validator (\_ -> Failure ["e1"]),
    Validator (\_ -> Failure ["e2"])
  ]

genValidatorIdx :: Gen Int
genValidatorIdx = Gen.int (Range.constant 0 (length validators - 1))

pickValidator :: Int -> Validator' [String] Int Int
pickValidator i = validators !! i

-- Semigroup / Monoid

mkAssoc :: (Validation [String] Int -> Validation [String] Int -> Validation [String] Int) -> Property
mkAssoc f =
  let g = forAll testGen
      assoc x y z = ((x `f` y) `f` z) === (x `f` (y `f` z))
   in property $ join (liftA3 assoc g g g)

prop_semigroup_assoc :: Property
prop_semigroup_assoc = mkAssoc (<>)

prop_monoid_assoc :: Property
prop_monoid_assoc = mkAssoc mappend

prop_monoid_left_id :: Property
prop_monoid_left_id =
  property $ do
    x <- forAll testGen
    (mempty `mappend` x) === x

prop_monoid_right_id :: Property
prop_monoid_right_id =
  property $ do
    x <- forAll testGen
    (x `mappend` mempty) === x

-- Functor

prop_functor_id :: Property
prop_functor_id =
  property $ do
    x <- forAll testGen
    fmap Prelude.id x === x

prop_functor_compose :: Property
prop_functor_compose =
  property $ do
    x <- forAll testGen
    let f = (+ 1)
        g = (* 2)
    fmap (f Prelude.. g) x === fmap f (fmap g x)

-- Applicative / Apply

prop_applicative_id :: Property
prop_applicative_id =
  property $ do
    x <- forAll testGen
    (pure Prelude.id <*> x) === x

prop_applicative_homomorphism :: Property
prop_applicative_homomorphism =
  property $ do
    x <- forAll genInt
    let f = (+ 1)
    (pure f <*> pure x :: Validation [String] Int) === pure (f x)

prop_apply_compose :: Property
prop_apply_compose =
  property $ do
    w <- forAll testGen
    let u = Success (+ 1) :: Validation [String] (Int -> Int)
        v = Success (* 2) :: Validation [String] (Int -> Int)
    (fmap (Prelude..) u <.> v <.> w) === (u <.> (v <.> w))

-- Alt

prop_alt_assoc :: Property
prop_alt_assoc =
  property $ do
    x <- forAll testGen
    y <- forAll testGen
    z <- forAll testGen
    ((x <!> y) <!> z) === (x <!> (y <!> z))

prop_alt_left_catch :: Property
prop_alt_left_catch =
  property $ do
    x <- forAll genInt
    y <- forAll testGen
    (Success x <!> y) === (Success x :: Validation [String] Int)

-- Bifunctor

prop_bifunctor_id :: Property
prop_bifunctor_id =
  property $ do
    x <- forAll testGen
    bimap Prelude.id Prelude.id x === x

prop_bifunctor_compose :: Property
prop_bifunctor_compose =
  property $ do
    x <- forAll testGen
    let f = (++ ["x"])
        g = (+ 1)
        h = (++ ["y"])
        k = (* 2)
    bimap (f Prelude.. h) (g Prelude.. k) x === bimap f g (bimap h k x)

-- foldValidation

prop_foldValidation_failure :: Property
prop_foldValidation_failure =
  property $ do
    e <- forAll genStrings
    foldValidation length (const 0) (Failure e :: Validation [String] Int) === length e

prop_foldValidation_success :: Property
prop_foldValidation_success =
  property $ do
    a <- forAll genInt
    foldValidation (const 0) (+ 1) (Success a :: Validation [String] Int) === (a + 1)

-- Iso: either

prop_either_roundtrip :: Property
prop_either_roundtrip =
  property $ do
    x <- forAll testGen
    (x ^. either ^. from either) === x

prop_either_roundtrip_inv :: Property
prop_either_roundtrip_inv =
  property $ do
    x <- forAll testGen
    let e = x ^. either :: Prelude.Either [String] Int
    (e ^. from either) === x

-- Iso: codiagonal

prop_codiagonal_roundtrip :: Property
prop_codiagonal_roundtrip =
  property $ do
    x <- forAll (genValidation genInt genInt)
    (x ^. codiagonal ^. from codiagonal) === x

-- Prisms

prop_failure_prism_review_preview :: Property
prop_failure_prism_review_preview =
  property $ do
    e <- forAll genStrings
    let v = review _Failure e :: Validation [String] Int
    v ^? _Failure === Just e

prop_success_prism_review_preview :: Property
prop_success_prism_review_preview =
  property $ do
    a <- forAll genInt
    let v = review _Success a :: Validation [String] Int
    v ^? _Success === Just a

prop_failure_prism_miss :: Property
prop_failure_prism_miss =
  property $ do
    a <- forAll genInt
    (Success a :: Validation [String] Int) ^? _Failure === Nothing

prop_success_prism_miss :: Property
prop_success_prism_miss =
  property $ do
    e <- forAll genStrings
    (Failure e :: Validation [String] Int) ^? _Success === Nothing

-- Polymorphic prisms

prop_poly_failure_prism :: Property
prop_poly_failure_prism =
  property $ do
    e <- forAll genStrings
    let v = __Failure # e :: Validation [String] Int
    v ^? __Failure === Just e

prop_poly_success_prism :: Property
prop_poly_success_prism =
  property $ do
    a <- forAll genInt
    let v = __Success # a :: Validation [String] Int
    v ^? __Success === Just a

-- Swap

prop_swap_failure :: Property
prop_swap_failure =
  property $ do
    e <- forAll genString
    let v = Failure e :: Validation String Int
    swap v === (Success e :: Validation Int String)

prop_swap_success :: Property
prop_swap_success =
  property $ do
    a <- forAll genInt
    let v = Success a :: Validation String Int
    swap v === (Failure a :: Validation Int String)

prop_swap_involution :: Property
prop_swap_involution =
  property $ do
    x <- forAll testGen
    (swap (swap x)) === x

-- Validator: Functor

prop_validator_functor_id :: Property
prop_validator_functor_id =
  property $ do
    x <- forAll genInt
    i <- forAll genValidatorIdx
    let v = pickValidator i
    runV (fmap Prelude.id v) x === runV v x

-- Validator: Category

prop_validator_category_left_id :: Property
prop_validator_category_left_id =
  property $ do
    x <- forAll genInt
    i <- forAll genValidatorIdx
    let v = pickValidator i
    runV (id . v) x === runV v x

prop_validator_category_right_id :: Property
prop_validator_category_right_id =
  property $ do
    x <- forAll genInt
    i <- forAll genValidatorIdx
    let v = pickValidator i
    runV (v . id) x === runV v x

prop_validator_category_assoc :: Property
prop_validator_category_assoc =
  property $ do
    x <- forAll genInt
    fi <- forAll genValidatorIdx
    gi <- forAll genValidatorIdx
    hi <- forAll genValidatorIdx
    let f = pickValidator fi
        g = pickValidator gi
        h = pickValidator hi
    runV ((f . g) . h) x === runV (f . (g . h)) x

-- Validator: Semigroupoid

prop_validator_semigroupoid_assoc :: Property
prop_validator_semigroupoid_assoc =
  property $ do
    x <- forAll genInt
    fi <- forAll genValidatorIdx
    gi <- forAll genValidatorIdx
    hi <- forAll genValidatorIdx
    let f = pickValidator fi
        g = pickValidator gi
        h = pickValidator hi
    runV ((f `o` g) `o` h) x === runV (f `o` (g `o` h)) x

-- Validator: Apply / Alt accumulate errors

prop_validator_apply_accumulates :: Property
prop_validator_apply_accumulates =
  property $ do
    x <- forAll genInt
    let f = Validator (\_ -> Failure ["e1"]) :: Validator' [String] Int Int
        g = Validator (\_ -> Failure ["e2"]) :: Validator' [String] Int Int
    runV (fmap const f <.> g) x === Failure ["e1", "e2"]

prop_validator_alt_accumulates :: Property
prop_validator_alt_accumulates =
  property $ do
    x <- forAll genInt
    let f = Validator (\_ -> Failure ["e1"]) :: Validator' [String] Int Int
        g = Validator (\_ -> Failure ["e2"]) :: Validator' [String] Int Int
    runV (f <!> g) x === Failure ["e1", "e2"]

prop_validator_alt_left_success :: Property
prop_validator_alt_left_success =
  property $ do
    x <- forAll genInt
    let f = Validator (Success . (+ 1)) :: Validator' [String] Int Int
        g = Validator (\_ -> Failure ["e2"]) :: Validator' [String] Int Int
    runV (f <!> g) x === Success (x + 1)

-- Either instances: ReviewFailure, AsFailure, ReviewSuccess, AsSuccess

genEither :: Gen a -> Gen b -> Gen (Prelude.Either a b)
genEither ga gb = Gen.choice [fmap Left ga, fmap Right gb]

prop_either_reviewFailure :: Property
prop_either_reviewFailure =
  property $ do
    e <- forAll genStrings
    (reviewFailure # e :: Prelude.Either [String] Int) === Left e

prop_either_asFailure_hit :: Property
prop_either_asFailure_hit =
  property $ do
    e <- forAll genStrings
    (Left e :: Prelude.Either [String] Int) ^? _Failure === Just e

prop_either_asFailure_miss :: Property
prop_either_asFailure_miss =
  property $ do
    a <- forAll genInt
    (Right a :: Prelude.Either [String] Int) ^? _Failure === Nothing

prop_either_reviewSuccess :: Property
prop_either_reviewSuccess =
  property $ do
    a <- forAll genInt
    (reviewSuccess # a :: Prelude.Either [String] Int) === Right a

prop_either_asSuccess_hit :: Property
prop_either_asSuccess_hit =
  property $ do
    a <- forAll genInt
    (Right a :: Prelude.Either [String] Int) ^? _Success === Just a

prop_either_asSuccess_miss :: Property
prop_either_asSuccess_miss =
  property $ do
    e <- forAll genStrings
    (Left e :: Prelude.Either [String] Int) ^? _Success === Nothing

prop_either_failure_roundtrip :: Property
prop_either_failure_roundtrip =
  property $ do
    x <- forAll (genEither genStrings genInt)
    let reviewed = x ^? _Failure
    case x of
      Left e -> reviewed === Just e
      Right _ -> reviewed === Nothing

prop_either_success_roundtrip :: Property
prop_either_success_roundtrip =
  property $ do
    x <- forAll (genEither genStrings genInt)
    let reviewed = x ^? _Success
    case x of
      Right a -> reviewed === Just a
      Left _ -> reviewed === Nothing
