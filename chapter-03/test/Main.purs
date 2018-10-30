module Test.Main where

import Prelude

import AddressBook (emptyBook, insertEntry, removeDuplicates)
import Data.List (length)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "AddressBook" do
    it "should accept entries and remove duplicates" do
        let address = { street: "123 Fake Street", city: "Stockholm", state: "Södermanland" }
        let entryA = { lastName: "Snow", firstName: "John", address: address }
        let entryB = { lastName: "Snow", firstName: "John", address: address }
        let entryC = { lastName: "Doe", firstName: "John", address: address }
        let newBook = insertEntry entryA emptyBook
        let newBook2 = insertEntry entryB newBook
        let newBook3 = insertEntry entryC newBook2
        let fixed = removeDuplicates newBook3
        length fixed `shouldEqual` 2