{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.HUnit

import Prelude hiding (length)
import Control.Lens ((#))
import Data.Foldable (length)
import Data.Proxy (Proxy (Proxy))
import Data.Validation (AccValidation (AccSuccess, AccFailure), Validate, _Failure, _Success, ensure,
                        orElse, validate, validation, validationNel)

seven :: Int
seven = 7

three :: Int
three = 3

testYY :: Test
testYY =
  let subject  = _Success # (+1) <*> _Success # seven :: AccValidation String Int
      expected = AccSuccess 8
  in  TestCase (assertEqual "Success <*> Success" subject expected)

testNY :: Test
testNY =
  let subject  = _Failure # ["f1"] <*> _Success # seven :: AccValidation [String] Int
      expected = AccFailure ["f1"]
  in  TestCase (assertEqual "Failure <*> Success" subject expected)

testYN :: Test
testYN =
  let subject  = _Success # (+1) <*> _Failure # ["f2"] :: AccValidation [String] Int
      expected = AccFailure ["f2"]
  in  TestCase (assertEqual "Success <*> Failure" subject expected)

testNN :: Test
testNN =
  let subject  = _Failure # ["f1"] <*> _Failure # ["f2"] :: AccValidation [String] Int
      expected = AccFailure ["f1","f2"]
  in  TestCase (assertEqual "Failure <*> Failure" subject expected)

testValidationNel :: Test
testValidationNel =
  let subject  = validation length (const 0) $ validationNel (Left ())
  in  TestCase (assertEqual "validationNel makes lists of length 1" subject 1)

testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
  testOrElseRight, testOrElseLeft
  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test

testEnsureLeftFalse _ =
  let subject :: v Int Int
      subject = ensure three (const False) (_Failure # seven)
  in  TestCase (assertEqual "ensure Left False" subject (_Failure # seven))

testEnsureLeftTrue _ =
  let subject :: v Int Int
      subject = ensure three (const True) (_Failure # seven)
  in  TestCase (assertEqual "ensure Left True" subject (_Failure # seven))

testEnsureRightFalse _ =
  let subject :: v Int Int
      subject = ensure three (const False) (_Success # seven)
  in  TestCase (assertEqual "ensure Right False" subject (_Failure # three))

testEnsureRightTrue _ =
  let subject :: v Int Int
      subject = ensure three (const True ) (_Success # seven)
  in  TestCase (assertEqual "ensure Right True" subject (_Success # seven))

testOrElseRight _ =
  let v :: v Int Int
      v = _Success # seven
      subject = v `orElse` three
  in  TestCase (assertEqual "orElseRight" subject seven)

testOrElseLeft _ =
  let v :: v Int Int
      v = _Failure # seven
      subject = v `orElse` three
  in  TestCase (assertEqual "orElseLeft" subject three)

testValidateTrue :: Test
testValidateTrue =
  let subject = validate three (const True) seven
      expected = AccSuccess seven
  in  TestCase (assertEqual "testValidateTrue" subject expected)

testValidateFalse :: Test
testValidateFalse =
  let subject = validate three (const True) seven
      expected = AccFailure three
  in  TestCase (assertEqual "testValidateFalse" subject expected)

tests :: Test
tests =
  let eitherP :: Proxy Either
      eitherP = Proxy
      validationP :: Proxy AccValidation
      validationP = Proxy
      generals :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => [Proxy v -> Test]
      generals =
        [ testEnsureLeftFalse
        , testEnsureLeftTrue
        , testEnsureRightFalse
        , testEnsureRightTrue
        , testOrElseLeft
        , testOrElseRight
        ]
      eithers = fmap ($ eitherP) generals
      validations = fmap ($ validationP) generals
  in  TestList $ [
    testYY
  , testYN
  , testNY
  , testNN
  , testValidationNel
  , testValidateFalse
  , testValidateTrue
  ] ++ eithers ++ validations
  where

main :: IO Counts
main = runTestTT tests

