module Data.Validation.Tests where

import Control.Applicative
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Semigroup
import Data.Validation
import Data.Traversable

instance (Arbitrary err, Arbitrary a) =>  Arbitrary (AccValidation err a) where
  arbitrary =
    fmap (either failure success) arbitrary

instance (Arbitrary err, Arbitrary a) =>  Arbitrary (Validation err a) where
  arbitrary =
    fmap (either failure success) arbitrary

instance (Applicative m, Arbitrary err, Arbitrary a) =>  Arbitrary (ValidationT m err a) where
  arbitrary =
    fmap (ValidationT . pure) arbitrary

main ::
  IO ()
main =
  defaultMain validationTests

validationTests ::
  [Test]
validationTests =
  [
    testGroup "Validation"
      [
        testProperty "AccValidation Semigroup associative"      prop_semigroup_associative_AccValidation
      , testProperty "AccValidation Monoid associative"      prop_monoid_associative_AccValidation
      , testProperty "AccValidation Monoid right-identity"      prop_monoid_right_identity_AccValidation
      , testProperty "AccValidation Monoid left-identity"      prop_monoid_left_identity_AccValidation
      ]
  ]

prop_semigroup_associative_AccValidation ::
  AccValidation [Int] String
  -> AccValidation [Int] String
  -> AccValidation [Int] String
  -> Bool
prop_semigroup_associative_AccValidation x y z =
  ((x <> y) <> z) == (x <> (y <> z))

prop_monoid_associative_AccValidation ::
  AccValidation [Int] String
  -> AccValidation [Int] String
  -> AccValidation [Int] String
  -> Bool
prop_monoid_associative_AccValidation x y z =
  ((x `mappend` y) `mappend` z) == (x `mappend` (y `mappend` z))

prop_monoid_right_identity_AccValidation ::
  AccValidation [Int] String
  -> Bool
prop_monoid_right_identity_AccValidation x =
  x `mappend` mempty == x

prop_monoid_left_identity_AccValidation ::
  AccValidation [Int] String
  -> Bool
prop_monoid_left_identity_AccValidation x =
  mempty `mappend` x == x
