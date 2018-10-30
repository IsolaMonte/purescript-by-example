module AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

-- 3. Functions and Records

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
  entry.firstName <> ": " <>
  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book


-- 3.12 Querying the Address Book
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
      -- head :: AddressBook -> Maybe Entry
      -- filter :: (Entry -> Boolean) -> AddressBook -> AddressBook
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


-- EXERCISES
{-
1. (Easy) Test your understanding of the findEntry function by writing down the types of each of its major subexpressions.
For example, the type of the head function as used is specialized to AddressBook -> Maybe Entry.
-}

{-
2. (Medium) Write a function which looks up an Entry given a street address,
by reusing the existing code in findEntry.
-}
findByAddress :: String -> AddressBook -> Maybe Entry
findByAddress street = head <<< filter filterAddress
  where
    filterAddress :: Entry -> Boolean
    filterAddress entry = entry.address.street == street

{-
3. (Medium) Write a function which tests whether a name appears in a AddressBook, returning a Boolean value.
Hint: Use PSCi to find the type of the Data.List.null function, which test whether a list is empty or not.
-}
entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName book = if (null $ filter filterEntry book) then false else true
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

{-
4. (Difficult) Write a function removeDuplicates which removes duplicate address book entries with the same first and last names.
Hint: Use PSCi to find the type of the Data.List.nubBy function, which removes duplicate elements from a list based on an equality predicate.
-}
-- nubBy :: (Entry -> Entry -> Boolean) -> AddressBook -> AddressBook
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy isDuplicate book

isDuplicate :: (Entry -> Entry -> Boolean)
isDuplicate entryA entryB = entryA.firstName == entryB.firstName && entryA.lastName == entryB.lastName


-- Filter based on predicate function
-- filter' (\x -> x > 2) (1..10)

filter' :: (Int -> Boolean) -> Array Int -> Array Int
filter' f arr = case uncons arr of
  Just {head: x, tail} -> if (f x) then x : (filter' f tail) else (filter' f tail)
  Nothing -> []

filter'' :: (Int -> Boolean) -> Array Int -> Array Int
filter'' f arr = case uncons arr of
  Just {head: x, tail} | f x -> x : filter'' f tail
  Just {tail} -> filter'' f tail
  Nothing -> []