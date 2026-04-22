module Main where

import Control.Lens (from, (#), (^.))
import Data.Bifoldable (bitraverse_)
import Data.Bifunctor (first, second)
import Data.Validation (Validation, either, foldValidation, _Failure, _Success)
import Prelude hiding (either)

main :: IO ()
main = do
  putStrLn "--- Creating Values ---"
  print successVal
  print (failureVal :: Validation Int String)

  putStrLn "\n--- Mapping ---"
  exMapping

  putStrLn "\n--- Folding ---"
  exFolding

  putStrLn "\n--- Catamorphism ---"
  exCatamorphism

  putStrLn "\n--- Converting ---"
  exConvert

-- Creating Values
--
-- Use the _Success and _Failure prisms

successVal :: Validation String String
successVal = _Success # "A"

failureVal :: Validation Int a
failureVal = _Failure # 5

-- | Mapping
--
-- The Validation type is a Functor over success values and a Bifunctor.
exMapping :: IO ()
exMapping = do
  print $ fmap (++ " B") successVal
  print $ second (++ " B") successVal
  print $ first (+ 1) (failureVal :: Validation Int String)

-- | Folding
--
-- Validation is Bifoldable and Bitraversable.
exFolding :: IO ()
exFolding = do
  bitraverse_ onFailure onSuccess successVal
  bitraverse_ onFailure onSuccess (failureVal :: Validation Int String)
  where
    onFailure v = putStrLn $ "Failure: " ++ show v
    onSuccess v = putStrLn $ "Success: " ++ v

-- | Catamorphism
--
-- foldValidation eliminates a Validation by providing handlers for both cases.
exCatamorphism :: IO ()
exCatamorphism = do
  putStrLn $ foldValidation ("Failed with: " ++) ("Succeeded with: " ++) successVal
  putStrLn $ foldValidation (\e -> "Failed with: " ++ show e) (\a -> "Succeeded with: " ++ show a) failureVal'
  where
    failureVal' :: Validation Int Int
    failureVal' = _Failure # 42

-- | Converting
--
-- The 'either' iso converts between Validation and Either.
-- 'from' reverses the direction.
exConvert :: IO ()
exConvert = do
  print (successVal ^. either :: Either String String)
  print ((failureVal :: Validation Int String) ^. either)
  print ((Right "hello" :: Either Int String) ^. from either)
  print ((Left 42 :: Either Int String) ^. from either)
