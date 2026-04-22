import System.Exit (ExitCode (..), exitFailure)
import System.Process (rawSystem)

main :: IO ()
main = do
  exit <-
    rawSystem
      "cabal"
      [ "exec",
        "--",
        "doctest",
        "-isrc",
        "src/Data/Validation.hs"
      ]
  case exit of
    ExitSuccess -> pure ()
    ExitFailure _ -> exitFailure
