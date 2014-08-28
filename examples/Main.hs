module Main where

import Control.Lens
import Data.Bifoldable
import Data.Bifunctor
import Data.Validation

main :: IO ()
main = putStrLn "5"

-- | Creating Values
-- Use the _Success and _Failure prisms

successAcc :: AccValidation e String
successAcc = _Success # "A"

successVal :: Validation e String
successVal = _Success # "A"

successValT :: Either e String
successValT = _Success # "A"

failureAcc :: AccValidation Int a
failureAcc = _Failure # 5

-- | Mapping
-- The validation types are Functors over the success values,
-- and are Bifunctors.
exMapping :: ()
exMapping =
    let -- fmap/second are equivalent, and map successes
        m1 = fmap (++ " B" ) successAcc
        m1' = second (++ " B") successAcc
        m2 = first (+1) failureAcc
        m2' = first (+1) successAcc -- does nothing

     in ()

-- | Folding
-- The Validation types are Bifoldable and Bitraversable
-- http://hackage.haskell.org/package/bifunctors-4.1.1.1/docs/Data-Bifoldable.html
-- These typeclasses have rich APIs, and would probably replace most usages of
-- pattern matching.

exFolding :: IO ()
exFolding = do
  bitraverse_ onFailure onSuccess successAcc
  -- OR
  bimapM_ onFailure onSuccess successAcc
    where onFailure _ = putStrLn "Some failure"
          onSuccess v = putStrLn $ "Good " ++ v

-- | Converting
-- There are isomorphisms between the validation types, and Either.
-- 'from' will reverse the isomorphism.

exConvert :: IO ()
exConvert = do
  print (successAcc ^. isoAccValidationEither :: Either Int String)
  print (successVal ^. isoValidationEither :: Either Int String)
  print (Left 3 ^. from isoValidationEither :: Validation Int String)
