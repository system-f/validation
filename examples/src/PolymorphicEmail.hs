-- Modification of the Email example that demonstrates converting
-- between Validation and Either using the 'either' isomorphism.
--
-- Validation accumulates all errors; Either short-circuits on first.

import Control.Lens ((#), (^.))
import Data.List (isInfixOf)
import Data.Validation
import Prelude hiding (either)

newtype Email = Email String deriving (Show)

data VError
  = MustNotBeEmpty
  | MustContainAt
  | MustContainPeriod
  deriving (Show)

-- ***** Base smart constructors *****

atString :: String -> Validation [VError] ()
atString x =
  if "@" `isInfixOf` x
    then _Success # ()
    else _Failure # [MustContainAt]

periodString :: String -> Validation [VError] ()
periodString x =
  if "." `isInfixOf` x
    then _Success # ()
    else _Failure # [MustContainPeriod]

nonEmptyString :: String -> Validation [VError] ()
nonEmptyString x =
  if x /= []
    then _Success # ()
    else _Failure # [MustNotBeEmpty]

-- ***** Combining smart constructors *****

email :: String -> Validation [VError] Email
email x =
  Email x
    <$ nonEmptyString x
    <* atString x
    <* periodString x

-- ***** Example usage *****

success :: Validation [VError] Email
success = email "bob@gmail.com"

failureAt :: Validation [VError] Email
failureAt = email "bobgmail.com"

failurePeriod :: Validation [VError] Email
failurePeriod = email "bob@gmailcom"

failureAll :: Validation [VError] Email
failureAll = email ""

main :: IO ()
main = do
  putStrLn "Collect all errors (Validation)"
  putStrLn $ "email \"bob@gmail.com\": " ++ show success
  putStrLn $ "email \"bobgmail.com\":  " ++ show failureAt
  putStrLn $ "email \"bob@gmailcom\":  " ++ show failurePeriod
  putStrLn $ "email \"\":              " ++ show failureAll
  putStrLn ""
  putStrLn "Convert to Either (stop at first error via Either's Monad)"
  putStrLn $ "email \"bob@gmail.com\": " ++ show (success ^. either)
  putStrLn $ "email \"\":              " ++ show (failureAll ^. either)
