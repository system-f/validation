module Person where

import Control.Lens
import Data.List (isInfixOf)
import Data.Validation

newtype Name = Name { unName :: String } deriving Show
newtype Email = Email { unEmail :: String } deriving Show
newtype Age = Age { unAge :: Int } deriving Show

data Person = Person { name :: Name
                     , email :: Email
                     , age :: Age
                     } deriving Show

data Error = NameBetween1And50
           | EmailMustContainAtChar
           | AgeBetween0and120
           deriving Show

-- Smart constructors
mkName :: String -> Validation [Error] Name
mkName s = let l = length s
           in if l >= 1 && l <= 50
              then _Success # Name s
              else _Failure # [ NameBetween1And50 ]


mkEmail :: String -> Validation [Error] Email
mkEmail s = if "@" `isInfixOf` s
            then _Success # Email s
            else _Failure # [ EmailMustContainAtChar ]

mkAge :: Int -> Validation [Error] Age
mkAge a = if a >= 0 && a <= 120
          then _Success # Age a
          else _Failure # [ AgeBetween0and120 ]

mkPerson :: String -> String -> Int -> Validation [Error] Person
mkPerson pName pEmail pAge =
  Person
  <$> mkName pName
  <*> mkEmail pEmail
  <*> mkAge pAge

-- Examples
-- Data constructors for `Name`, `Age`, `Email`, and `Person` should not be
-- exported to the example code below:

validPerson :: Validation [Error] Person
validPerson = mkPerson "Bob" "bob@gmail.com" 25
-- Success (Person {name = Name {unName = "Bob"}, email = Email {unEmail = "bob@gmail.com"}, age = Age {unAge = 25}})

badName :: Validation [Error] Person
badName = mkPerson "" "bob@gmail.com" 25
-- Failure [NameBetween1And50]

badEmail :: Validation [Error] Person
badEmail = mkPerson "Bob" "bademail" 25
-- Failure [EmailMustContainAtChar]

badAge :: Validation [Error] Person
badAge = mkPerson "Bob" "bob@gmail.com" 150
-- Failure [AgeBetween0and120]

badEverything :: Validation [Error] Person
badEverything = mkPerson "" "bademail" 150
-- Failure [NameBetween1And50,EmailMustContainAtChar,AgeBetween0and120]

asMaybeGood :: Maybe Person
asMaybeGood = validPerson ^? _Success
-- Just (Person {name = Name {unName = "Bob"}, email = Email {unEmail = "bob@gmail.com"}, age = Age {unAge = 25}})

asMaybeBad :: Maybe Person
asMaybeBad = badEverything ^? _Success
-- Nothing

asEitherGood :: Either [Error] Person
asEitherGood = validPerson ^. _Either
-- Right (Person {name = Name {unName = "Bob"}, email = Email {unEmail = "bob@gmail.com"}, age = Age {unAge = 25}})

asEitherBad :: Either [Error] Person
asEitherBad = badEverything ^. _Either
-- Left [NameBetween1And50,EmailMustContainAtChar,AgeBetween0and120]

main :: IO ()
main = do
  putStrLn $ "validPerson:   " ++ show validPerson
  putStrLn $ "badName:       " ++ show badName
  putStrLn $ "badEmail:      " ++ show badEmail
  putStrLn $ "badAge:        " ++ show badAge
  putStrLn $ "badEverything: " ++ show badEverything
  putStrLn $ "asMaybeGood:   " ++ show asMaybeGood
  putStrLn $ "asMaybeBad:    " ++ show asMaybeBad
  putStrLn $ "asEitherGood:  " ++ show asEitherGood
  putStrLn $ "asEitherBad:   " ++ show asEitherBad
