{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA3)
import Control.Monad (join, unless)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import System.Exit (exitFailure)

import Data.Validation (Validation (Success, Failure))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  result <- checkParallel $ Group "Validation"
    [ ("prop_semigroup", prop_semigroup)
    , ("prop_monoid_assoc", prop_monoid_assoc)
    , ("prop_monoid_left_id", prop_monoid_left_id)
    , ("prop_monoid_right_id", prop_monoid_right_id)
    ]

  unless result $
    exitFailure

genValidation :: Gen e -> Gen a -> Gen (Validation e a)
genValidation e a = Gen.choice [fmap Failure e, fmap Success a]

testGen :: Gen (Validation [String] Int)
testGen =
  let range = Range.linear 1 50
      string = Gen.string range Gen.unicode
      strings = Gen.list range string
  in  genValidation strings Gen.enumBounded

mkAssoc :: (Validation [String] Int -> Validation [String] Int -> Validation [String] Int) -> Property
mkAssoc f =
  let g = forAll testGen
      assoc = \x y z -> ((x `f` y) `f` z) === (x `f` (y `f` z))
  in  property $ join (liftA3 assoc g g g)

prop_semigroup :: Property
prop_semigroup = mkAssoc (<>)

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

