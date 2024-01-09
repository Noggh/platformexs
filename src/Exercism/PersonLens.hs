{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Exercism.PersonLens (bornStreet, setCurrentStreet, setBirthMonth, renameStreets,Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)) where

import           Control.Lens       (Field2 (_2), makeLenses, over, (&), (.~),
                                     (^.))
import           Data.Time.Calendar (Day (..), fromGregorian, toGregorian)

{-
Instructions
Use lenses to update nested records (specific to languages with immutable data).

Updating fields of nested, immutable records is kind of annoying. 
The code for such cases is as cumbersome as the structure is deep. 
If you have, say, a Person, 
that contains an Address, 
which has a Street, 
that has a Number, 

updating the Number requires creating a new Street with the new Number, 
then a new Address with the new Street and, finally, 
a new Person with the new Address. Confused already?

One solution to this problem is to use lenses.

Implement several record accessing functions using lenses. 
The test suite also allows you to avoid lenses altogether so you can experiment with different approaches.
-}

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       } deriving (Show)
makeLenses ''Address

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 } deriving (Show)
makeLenses ''Born

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 } deriving (Show)
makeLenses ''Name

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     } deriving (Show)
makeLenses ''Person

bornStreet born = born ^. (bornAt . street)

setCurrentStreet s person = person & (address . street .~ s)

setBirthMonth m = over (born . bornOn) setMonth
    where setMonth d = let (y, m', d') = toGregorian d & _2 .~ m
                        in fromGregorian y m' d'

renameStreets f person = over (address . street)       f $
                         over (born . bornAt . street) f person
