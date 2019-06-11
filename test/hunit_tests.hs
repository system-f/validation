{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.HUnit

import Prelude hiding (length)
import Control.Lens ((#))
import Control.Monad (when)
import Data.Foldable (length)
import Data.Proxy (Proxy (Proxy))
import Data.Validation (Validation (Success, Failure), Validate, _Failure, _Success, ensure,
                        orElse, validate, validation, validationNel)
import System.Exit (exitFailure)

seven :: Int
seven = 7

three :: Int
three = 3

four :: Int
four = 4

testYY :: Test
testYY =
  let subject  = _Success # (+1) <*> _Success # seven :: Validation String Int
      expected = Success 8
  in  TestCase (assertEqual "Success <*> Success" subject expected)

testNY :: Test
testNY =
  let subject  = _Failure # ["f1"] <*> _Success # seven :: Validation [String] Int
      expected = Failure ["f1"]
  in  TestCase (assertEqual "Failure <*> Success" subject expected)

testYN :: Test
testYN =
  let subject  = _Success # (+1) <*> _Failure # ["f2"] :: Validation [String] Int
      expected = Failure ["f2"]
  in  TestCase (assertEqual "Success <*> Failure" subject expected)

testNN :: Test
testNN =
  let subject  = _Failure # ["f1"] <*> _Failure # ["f2"] :: Validation [String] Int
      expected = Failure ["f1","f2"]
  in  TestCase (assertEqual "Failure <*> Failure" subject expected)

testValidationNel :: Test
testValidationNel =
  let subject  = validation length (const 0) $ validationNel (Left ())
  in  TestCase (assertEqual "validationNel makes lists of length 1" subject 1)

testEnsureLeftNothing, testEnsureLeftJust, testEnsureRightNothing,
 testEnsureRightJust, testEnsureRightJust', testOrElseRight, testOrElseLeft
  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test

testEnsureLeftNothing _ =
  let subject :: v Int Int
      subject = ensure three (const Nothing) (_Failure # seven)
  in  TestCase (assertEqual "ensure Left False" subject (_Failure # seven))

testEnsureLeftJust _ =
  let subject :: v Int Int
      subject = ensure three (Just . id) (_Failure # seven)
  in  TestCase (assertEqual "ensure Left True" subject (_Failure # seven))

testEnsureRightNothing _ =
  let subject :: v Int Int
      subject = ensure three (const Nothing) (_Success # seven)
  in  TestCase (assertEqual "ensure Right False" subject (_Failure # three))

testEnsureRightJust _ =
  let subject :: v Int Int
      subject = ensure three (Just . id) (_Success # seven)
  in  TestCase (assertEqual "ensure Right True" subject (_Success # seven))

testEnsureRightJust' _ =
  let subject :: v Int Int
      subject = ensure three (const $ Just four) (_Success # seven)
  in  TestCase (assertEqual "ensure Right True" subject (_Success # four))

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

testValidateJust :: Test
testValidateJust =
  let subject = validate three (Just . id) seven
      expected = Success seven
  in  TestCase (assertEqual "testValidateTrue" subject expected)

testValidateJust' :: Test
testValidateJust' =
  let subject = validate three (const $ Just four) seven
      expected = Success four
  in  TestCase (assertEqual "testValidateTrue" subject expected)

testValidateNothing :: Test
testValidateNothing =
  let subject = validate three (const option) seven
      expected = Failure three
      option = Nothing :: Maybe Int
  in  TestCase (assertEqual "testValidateFalse" subject expected)

testMappendNY :: Test
testMappendNY =
  let v1 = _Failure # [three]
      v2 = _Success # [seven]
      subject = v1 <> v2
      expected = Failure [three]
  in  TestCase (assertEqual "Failure <> Success" subject expected)

testMappendYN :: Test
testMappendYN =
  let v1 = _Success # [three]
      v2 = _Failure # [seven]
      subject = v1 <> v2
      expected = Failure [seven]
  in  TestCase (assertEqual "Success <> Failure" subject expected)

testMappendYY :: Test
testMappendYY =
  let v1 = _Success # [three]
      v2 = _Success # [seven]
      subject = v1 <> v2 :: Validation [Int] [Int]
      expected = Success [three]
  in  TestCase (assertEqual "Success <> Success" subject expected)

testMappendNN :: Test
testMappendNN =
  let v1 = _Failure # [three]
      v2 = _Failure # [seven]
      subject = v1 <> v2 :: Validation [Int] [Int]
      expected = Failure [three, seven]
  in  TestCase (assertEqual "Failure <> Failure" subject expected)

tests :: Test
tests =
  let eitherP :: Proxy Either
      eitherP = Proxy
      validationP :: Proxy Validation
      validationP = Proxy
      generals :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => [Proxy v -> Test]
      generals =
        [ testEnsureLeftNothing
        , testEnsureLeftJust
        , testEnsureRightNothing
        , testEnsureRightJust
        , testEnsureRightJust' 
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
  , testValidateNothing
  , testValidateJust
  , testValidateJust'
  , testMappendNY
  , testMappendYY
  , testMappendNN
  , testMappendYN
  ] ++ eithers ++ validations
  where

main :: IO ()
main = do
  c <- runTestTT tests
  when (errors c > 0 || failures c > 0) exitFailure
