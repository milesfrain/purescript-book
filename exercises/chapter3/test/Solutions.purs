module Test.Solutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
-- Equivalent: findEntryByStreet streetName book = head $ filter filterEntry book
findEntryByStreet streetName = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry e = e.address.street == streetName

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = not null $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- This function is not written
removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates firstName lastName book = book
