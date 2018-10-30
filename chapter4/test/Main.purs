module Test.Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Data.Array (length)
import Data.Maybe (Maybe(..))

import Main (triples, factorsWithGuard, isPrime, cartesianProduct, isTrue, allTrue, onlyFiles, smallestFile)
import Data.Path (root)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Factors of n" do
    it "should compute factors correctly" do
      factorsWithGuard 3 `shouldEqual` [[1, 3]]
  describe "Prime number" do
    it "should determine prime number status correctly" do
      isPrime 0 `shouldEqual` false
      isPrime 2 `shouldEqual` true
      isPrime 7 `shouldEqual` true
      isPrime 9 `shouldEqual` false
  describe "Cartesian product" do
    it "should compute it correctly" do
      cartesianProduct [1,2] [3,4] `shouldEqual` [[1,3],[1,4],[2,3],[2,4]]
      cartesianProduct [3,4] [1,2] `shouldEqual` [[3,1],[3,2],[4,1],[4,2]]
  describe "Pythagorean triples" do
    it "should compute them correctly" do
      triples 5 `shouldEqual` [[3, 4, 5]]
      triples 13 `shouldEqual` [[5, 12, 13]]
      triples 17 `shouldEqual` [[8, 15, 17]]
      triples 25 `shouldNotEqual` [[7, 24, 25]]
  describe "Boolean foldl" do
    it "should tell whether the array is all true or not" do
      isTrue [true, true, false] `shouldEqual` false
      isTrue [true, true, true] `shouldEqual` true
  describe "Characterize arrays" do
    it "should return false for empty array" do
      allTrue [] `shouldEqual` false
    it "should return false for all true array" do
      allTrue [true, true, true] `shouldEqual` false
  describe "File system" do
    it "should list only files" do
      (length $ onlyFiles root) `shouldEqual` 7
