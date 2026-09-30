{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (liftA3)
import Control.Lens (from, matching, review, (#), (^.), (^?), _Just, _Left, _Right)
import Control.Monad (join, unless)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Swap (swap)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Identity (Identity (..))
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
        [ ("prop_semigroup_assoc", prop_semigroup_assoc)
        , ("prop_monoid_assoc", prop_monoid_assoc)
        , ("prop_monoid_left_id", prop_monoid_left_id)
        , ("prop_monoid_right_id", prop_monoid_right_id)
        , ("prop_functor_id", prop_functor_id)
        , ("prop_functor_compose", prop_functor_compose)
        , ("prop_applicative_id", prop_applicative_id)
        , ("prop_applicative_homomorphism", prop_applicative_homomorphism)
        , ("prop_apply_compose", prop_apply_compose)
        , ("prop_alt_assoc", prop_alt_assoc)
        , ("prop_alt_left_catch", prop_alt_left_catch)
        , ("prop_bifunctor_id", prop_bifunctor_id)
        , ("prop_bifunctor_compose", prop_bifunctor_compose)
        , ("prop_foldValidation_failure", prop_foldValidation_failure)
        , ("prop_foldValidation_success", prop_foldValidation_success)
        , ("prop_either_roundtrip", prop_either_roundtrip)
        , ("prop_either_roundtrip_inv", prop_either_roundtrip_inv)
        , ("prop_codiagonal_roundtrip", prop_codiagonal_roundtrip)
        , ("prop_failure_prism_review_preview", prop_failure_prism_review_preview)
        , ("prop_success_prism_review_preview", prop_success_prism_review_preview)
        , ("prop_failure_prism_miss", prop_failure_prism_miss)
        , ("prop_success_prism_miss", prop_success_prism_miss)
        , ("prop_poly_failure_prism", prop_poly_failure_prism)
        , ("prop_poly_success_prism", prop_poly_success_prism)
        , ("prop_swap_failure", prop_swap_failure)
        , ("prop_swap_success", prop_swap_success)
        , ("prop_swap_involution", prop_swap_involution)
        , ("prop_either_reviewFailure", prop_either_reviewFailure)
        , ("prop_either_asFailure_hit", prop_either_asFailure_hit)
        , ("prop_either_asFailure_miss", prop_either_asFailure_miss)
        , ("prop_either_reviewSuccess", prop_either_reviewSuccess)
        , ("prop_either_asSuccess_hit", prop_either_asSuccess_hit)
        , ("prop_either_asSuccess_miss", prop_either_asSuccess_miss)
        , ("prop_either_failure_roundtrip", prop_either_failure_roundtrip)
        , ("prop_either_success_roundtrip", prop_either_success_roundtrip)
        , ("prop_match_hit", prop_match_hit)
        , ("prop_match_miss", prop_match_miss)
        , ("prop_match_validatorProfunctor", prop_match_validatorProfunctor)
        , ("prop_match_validatorMonad", prop_match_validatorMonad)
        , ("prop_match_validatorMonadProfunctor", prop_match_validatorMonadProfunctor)
        , ("prop_match_alt", prop_match_alt)
        , ("prop_matchValidator_alt", prop_matchValidator_alt)
        , ("prop_matchValidatorProfunctor_alt", prop_matchValidatorProfunctor_alt)
        , ("prop_matchValidatorMonad_alt", prop_matchValidatorMonad_alt)
        , ("prop_matchValidatorMonadProfunctor_alt", prop_matchValidatorMonadProfunctor_alt)
        , ("prop_arrow_fmap_match", prop_arrow_fmap_match)
        , ("prop_arrow_id_match", prop_arrow_id_match)
        , ("prop_arrow_validator_alt", prop_arrow_validator_alt)
        , ("prop_arrow_validatorProfunctor_alt", prop_arrow_validatorProfunctor_alt)
        , ("prop_arrow_validatorMonad_alt", prop_arrow_validatorMonad_alt)
        , ("prop_arrow_validatorMonadProfunctor_alt", prop_arrow_validatorMonadProfunctor_alt)
        , ("prop_unmatch_match_matching", prop_unmatch_match_matching)
        , ("prop_unmatch_match_review", prop_unmatch_match_review)
        , ("prop_match_unmatch", prop_match_unmatch)
        , ("prop_unmatch_validatorProfunctor", prop_unmatch_validatorProfunctor)
        , ("prop_unmatch_validatorMonad", prop_unmatch_validatorMonad)
        , ("prop_unmatch_validatorMonadProfunctor", prop_unmatch_validatorMonadProfunctor)
        , ("prop_unmatch_arrow", prop_unmatch_arrow)
        , ("prop_unmatch_arrow_alt", prop_unmatch_arrow_alt)
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

-- match

matchRight :: Validator (Prelude.Either [String] Int) (Prelude.Either [String] Int) Int
matchRight = match _Right

runValidator :: Validator x err a -> x -> Validation err a
runValidator (Validator f) = f

prop_match_hit :: Property
prop_match_hit =
  property $ do
    a <- forAll genInt
    runValidator matchRight (Right a) === Success a

prop_match_miss :: Property
prop_match_miss =
  property $ do
    e <- forAll genStrings
    runValidator matchRight (Left e) === Failure (Left e)

prop_match_validatorProfunctor :: Property
prop_match_validatorProfunctor =
  property $ do
    x <- forAll (genEither genStrings genInt)
    let ValidatorProfunctor f = match _Right :: ValidatorProfunctor (Prelude.Either [String] Int) (Prelude.Either [String] Int) Int
    f x === runValidator matchRight x

prop_match_validatorMonad :: Property
prop_match_validatorMonad =
  property $ do
    x <- forAll (genEither genStrings genInt)
    let ValidatorMonadT f = match _Right :: ValidatorMonad (Prelude.Either [String] Int) (Prelude.Either [String] Int) Int
        ValidationMonadT (Identity r) = f x
    r === runValidator matchRight x

prop_match_validatorMonadProfunctor :: Property
prop_match_validatorMonadProfunctor =
  property $ do
    x <- forAll (genEither genStrings genInt)
    let ValidatorMonadProfunctorT f = match _Right :: ValidatorMonadProfunctor (Prelude.Either [String] Int) (Prelude.Either [String] Int) Int
        ValidationMonadT (Identity r) = f x
    r === runValidator matchRight x

-- match with (<!>): one prism per constructor, the first match wins

-- | The input used by the (<!>) properties: a Left, a Right Just, or a Right Nothing.
type Input = Prelude.Either String (Maybe Int)

genInput :: Gen Input
genInput = genEither genString (Gen.maybe genInt)

-- | The expected result: Left and Right Just match, Right Nothing matches neither prism.
expected :: Input -> Validation Input String
expected (Left s) = Success s
expected (Right (Just n)) = Success (show n)
expected i@(Right Nothing) = Failure i

prop_match_alt :: Property
prop_match_alt =
  property $ do
    i <- forAll genInput
    let v = match _Left <!> (show <$> match (_Right Prelude.. _Just)) :: Validator Input Input String
    runValidator v i === expected i

prop_matchValidator_alt :: Property
prop_matchValidator_alt =
  property $ do
    i <- forAll genInput
    let v = matchValidator _Left <!> (show <$> matchValidator (_Right Prelude.. _Just))
    runValidator v i === expected i

prop_matchValidatorProfunctor_alt :: Property
prop_matchValidatorProfunctor_alt =
  property $ do
    i <- forAll genInput
    let ValidatorProfunctor f = matchValidatorProfunctor _Left <!> (show <$> matchValidatorProfunctor (_Right Prelude.. _Just))
    f i === expected i

prop_matchValidatorMonad_alt :: Property
prop_matchValidatorMonad_alt =
  property $ do
    i <- forAll genInput
    let ValidatorMonadT f = matchValidatorMonad _Left <!> (show <$> matchValidatorMonad (_Right Prelude.. _Just))
        ValidationMonadT (Identity r) = f i
    r === expected i

prop_matchValidatorMonadProfunctor_alt :: Property
prop_matchValidatorMonadProfunctor_alt =
  property $ do
    i <- forAll genInput
    let ValidatorMonadProfunctorT f = matchValidatorMonadProfunctor _Left <!> (show <$> matchValidatorMonadProfunctor (_Right Prelude.. _Just))
        ValidationMonadT (Identity r) = f i
    r === expected i

-- (-->): match a prism and map its focus, one case per constructor

prop_arrow_fmap_match :: Property
prop_arrow_fmap_match =
  property $ do
    i <- forAll genInput
    let v = _Right Prelude.. _Just --> show :: Validator Input Input String
    runValidator v i === runValidator (show <$> matchValidator (_Right Prelude.. _Just)) i

prop_arrow_id_match :: Property
prop_arrow_id_match =
  property $ do
    i <- forAll genInput
    let v = _Left --> Prelude.id :: Validator Input Input String
    runValidator v i === runValidator (matchValidator _Left) i

prop_arrow_validator_alt :: Property
prop_arrow_validator_alt =
  property $ do
    i <- forAll genInput
    let v = _Left --> Prelude.id <!> _Right Prelude.. _Just --> show :: Validator Input Input String
    runValidator v i === expected i

prop_arrow_validatorProfunctor_alt :: Property
prop_arrow_validatorProfunctor_alt =
  property $ do
    i <- forAll genInput
    let ValidatorProfunctor f = _Left --> Prelude.id <!> _Right Prelude.. _Just --> show :: ValidatorProfunctor Input Input String
    f i === expected i

prop_arrow_validatorMonad_alt :: Property
prop_arrow_validatorMonad_alt =
  property $ do
    i <- forAll genInput
    let ValidatorMonadT f = _Left --> Prelude.id <!> _Right Prelude.. _Just --> show :: ValidatorMonad Input Input String
        ValidationMonadT (Identity r) = f i
    r === expected i

prop_arrow_validatorMonadProfunctor_alt :: Property
prop_arrow_validatorMonadProfunctor_alt =
  property $ do
    i <- forAll genInput
    let ValidatorMonadProfunctorT f = _Left --> Prelude.id <!> _Right Prelude.. _Just --> show :: ValidatorMonadProfunctor Input Input String
        ValidationMonadT (Identity r) = f i
    r === expected i

-- unmatch and (<--): construct a prism from a review and a validator

-- | A validator that succeeds on a positive number, and fails with its input otherwise.
positive :: Validator Int Int Int
positive = Validator (\n -> if n > 0 then Success n else Failure n)

-- | The expected match of positive.
expectedPositive :: Int -> Prelude.Either Int Int
expectedPositive n = if n > 0 then Right n else Left n

prop_unmatch_match_matching :: Property
prop_unmatch_match_matching =
  property $ do
    x <- forAll (genEither genStrings genInt)
    matching (unmatch _Right matchRight) x === matching _Right x

prop_unmatch_match_review :: Property
prop_unmatch_match_review =
  property $ do
    a <- forAll genInt
    review (unmatch _Right matchRight) a === (review _Right a :: Prelude.Either [String] Int)

prop_match_unmatch :: Property
prop_match_unmatch =
  property $ do
    n <- forAll genInt
    runValidator (matchValidator (unmatch Prelude.id positive)) n === runValidator positive n

prop_unmatch_validatorProfunctor :: Property
prop_unmatch_validatorProfunctor =
  property $ do
    n <- forAll genInt
    matching (unmatch Prelude.id (positive ^. validatorProfunctor)) n === expectedPositive n

prop_unmatch_validatorMonad :: Property
prop_unmatch_validatorMonad =
  property $ do
    n <- forAll genInt
    matching (unmatch Prelude.id (positive ^. validatorMonadT)) n === expectedPositive n

prop_unmatch_validatorMonadProfunctor :: Property
prop_unmatch_validatorMonadProfunctor =
  property $ do
    n <- forAll genInt
    matching (unmatch Prelude.id (positive ^. validatorMonadProfunctorT)) n === expectedPositive n

prop_unmatch_arrow :: Property
prop_unmatch_arrow =
  property $ do
    n <- forAll genInt
    matching (Prelude.id <-- positive) n === matching (unmatch Prelude.id positive) n

prop_unmatch_arrow_alt :: Property
prop_unmatch_arrow_alt =
  property $ do
    i <- forAll genInput
    let p = _Left <-- matchValidator _Left <!> show <$> matchValidator (_Right Prelude.. _Just)
    matching p i === expected i ^. either
