module Main where

import qualified Data.Validation.Tests
import Test.Framework

main ::
  IO ()
main = 
  defaultMain tests 

tests ::
  [Test]
tests =
  [
    testGroup "Tests" Data.Validation.Tests.validationTests
  ]

