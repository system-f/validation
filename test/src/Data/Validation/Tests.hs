module Data.Validation.Tests where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Validation
import Data.Pointed

instance (Arbitrary err, Arbitrary a) =>  Arbitrary (Validation err a) where
  arbitrary =
    fmap (either failure success) arbitrary

instance (Arbitrary err, Arbitrary a) =>  Arbitrary (AccValidation err a) where
  arbitrary =
    fmap (either failure success) arbitrary

instance (Pointed m, Arbitrary err, Arbitrary a) =>  Arbitrary (ValidationT m err a) where
  arbitrary =
    fmap (ValidationT . point) arbitrary

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
        testProperty "fold/success/failure for Validation"       prop_fold_success_failure_Validation
      , testProperty "fold/success/failure for AccValidation"    prop_fold_success_failure_AccValidation
      ]
  ]

prop_fold_success_failure ::
  (Validate v, FoldValidate v, Eq (v err a)) =>
  v err a
  -> Bool
prop_fold_success_failure v =
  foldValidate failure success v == v

prop_fold_success_failure_Validation ::
  Validation Int String
  -> Bool
prop_fold_success_failure_Validation =
  prop_fold_success_failure

prop_fold_success_failure_AccValidation ::
  AccValidation Int String
  -> Bool
prop_fold_success_failure_AccValidation =
  prop_fold_success_failure

