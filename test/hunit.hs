import Test.HUnit

import Control.Lens ((#))
import Data.Validation (AccValidation (AccSuccess, AccFailure), _Failure, _Success)

seven :: Int
seven = 7

test1 :: Test
test1 =
  let subject  = _Success # (+1) <*> _Success # seven :: AccValidation String Int
      expected = AccSuccess 8
  in  TestCase (assertEqual "Success <*> Success" subject expected)

test2 :: Test
test2 =
  let subject  = _Failure # ["f1"] <*> _Success # seven :: AccValidation [String] Int
      expected = AccFailure ["f1"]
  in  TestCase (assertEqual "Failure <*> Success" subject expected)

test3 :: Test
test3 =
  let subject  = _Success # (+1) <*> _Failure # ["f2"] :: AccValidation [String] Int
      expected = AccFailure ["f2"]
  in  TestCase (assertEqual "Success <*> Failure" subject expected)

test4 :: Test
test4 =
  let subject  = _Failure # ["f1"] <*> _Failure # ["f2"] :: AccValidation [String] Int
      expected = AccFailure ["f1","f2"]
  in  TestCase (assertEqual "Failure <*> Failure" subject expected)

tests :: Test
tests =
  TestList [
    test1
  , test2
  , test3
  , test4
  ]

main :: IO Counts
main = runTestTT tests

