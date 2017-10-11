-- Modification of the Email example, that leaves the validation
-- functions polymorphic.

-- This lets us choose whether to accumulate all errors, by specialising
-- to AccValidation, or abort on the first error with Either.

-- Aside from main, the code is unchanged but the type signatures have
-- been relaxed to be as polymorphic as possible.

import Prelude

import Control.Applicative
import Control.Lens
import Data.List (isInfixOf)
import Data.Validation

newtype Email = Email String deriving (Show)

data VError = MustNotBeEmpty
            | MustContainAt
            | MustContainPeriod
            deriving (Show)

-- ***** Base smart constructors *****
-- String must contain an '@' character
atString :: Validate f => String -> f [VError] ()
atString x = if "@" `isInfixOf` x
             then _Success # ()
             else _Failure # [MustContainAt]

-- String must contain an '.' character
periodString :: Validate f => String -> f [VError] ()
periodString x = if "." `isInfixOf` x
                 then _Success # ()
                 else _Failure # [MustContainPeriod]

-- String must not be empty
nonEmptyString :: Validate f => String -> f [VError] ()
nonEmptyString x = if x /= []
                   then _Success # ()
                   else _Failure # [MustNotBeEmpty]

-- ***** Combining smart constructors *****
email :: (Validate f, Applicative (f [VError])) => String -> f [VError] Email
email x = pure (Email x)   <*
          nonEmptyString x <*
          atString       x <*
          periodString   x

-- ***** Example usage *****
success :: (Applicative (f [VError]), Validate f) => f [VError] Email
success = email "bob@gmail.com"
-- AccSuccess (Email "bob@gmail.com")

failureAt :: (Applicative (f [VError]), Validate f) => f [VError] Email
failureAt = email "bobgmail.com"
-- AccFailure [MustContainAt]

failurePeriod :: (Applicative (f [VError]), Validate f) => f [VError] Email
failurePeriod = email "bob@gmailcom"
-- AccFailure [MustContainPeriod]

failureAll :: (Applicative (f [VError]), Validate f) => f [VError] Email
failureAll = email ""
-- AccFailure [MustNotBeEmpty,MustContainAt,MustContainPeriod]


-- Helper to force a validation to AccValidation
asAcc :: AccValidation a b -> AccValidation a b
asAcc = id

-- Helper to force a validation to Validation
asEither :: Either a b -> Either a b
asEither = id

main :: IO ()
main = do
  putStrLn "Collect all errors"
  putStrLn $ "email \"bob@gmail.com\": " ++ show (asAcc success)
  putStrLn $ "email \"bobgmail.com\":  " ++ show (asAcc failureAt)
  putStrLn $ "email \"bob@gmailcom\":  " ++ show (asAcc failurePeriod)
  putStrLn $ "email \"\":              " ++ show (asAcc failureAll)
  putStrLn "Stop at the first error"
  putStrLn $ "email \"bob@gmail.com\": " ++ show (asEither success)
  putStrLn $ "email \"\":              " ++ show (asEither failureAll)
