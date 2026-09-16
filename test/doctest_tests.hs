import Control.Monad (unless)
import System.Exit (ExitCode (..), exitFailure)
import System.Process (rawSystem)

main :: IO ()
main = do
  results <-
    mapM
      ( \f ->
          rawSystem
            "cabal"
            [ "exec"
            , "--"
            , "doctest"
            , "-isrc"
            , f
            ]
      )
      [ "src/Data/Validation/Validation.hs"
      , "src/Data/Validation/ValidationMonad.hs"
      , "src/Data/Validation/Validator.hs"
      ]
  unless (all (== ExitSuccess) results) exitFailure
