module Main where

import Control.Lens.Getter((^.))
import Control.Lens.Iso(from)
import Control.Lens.Review(( # ))
import Data.Bifoldable(bitraverse_, bimapM_)
import Data.Bifunctor(second, first)
import Data.Validation(Validation, _Validation, _Either, _Success, _Failure)

main ::
  IO ()
main =
  putStrLn "5"

-- Creating Values
--
-- Use the _Success and _Failure prisms

successVal ::
  Validation e String
successVal =
  _Success # "A"

successEither ::
  Either e String
successEither =
  _Success # "A"

failureVal ::
  Validation Int a
failureVal =
  _Failure # 5

-- | Mapping
--
-- The validation types are Functors over the success values,
-- and are Bifunctors.
exMapping ::
  ()
exMapping =
  let -- fmap/second are equivalent, and map successes
      _ = fmap (++ " B" ) successVal
      _ = second (++ " B") successVal
      _ = first (+1) failureVal
      _ = first (+(1 :: Integer)) successVal -- does nothing
   in ()

-- | Folding
--
-- The Validation types are Bifoldable and Bitraversable
-- http://hackage.haskell.org/package/bifunctors-4.1.1.1/docs/Data-Bifoldable.html
-- These typeclasses have rich APIs, and would probably replace most usages of
-- pattern matching.

exFolding ::
  IO ()
exFolding =
  do
      bitraverse_ onFailure onSuccess successVal
      -- OR
      bimapM_ onFailure onSuccess successVal
        where onFailure _ = putStrLn "Some failure"
              onSuccess v = putStrLn $ "Good " ++ v

-- | Converting
-- There are isomorphisms between the validation types, and Either.
-- 'from' will reverse the isomorphism.

exConvert ::
  IO ()
exConvert =
  do
      print (successVal ^. from _Validation :: Either Int String)
      print (successEither ^. from _Either :: Validation Int String)
