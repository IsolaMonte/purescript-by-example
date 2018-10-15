module Main where

import Prelude (Unit, show, ($), (>))

import Effect (Effect)
import Effect.Console (log)
import Data.Array (uncons, (..), (:))
import Data.Maybe

filter' :: (Int -> Boolean) -> Array Int -> Array Int
filter' f arr = case uncons arr of
  Just {head: x, tail} -> if (f x) then x : (filter' f tail) else (filter' f tail)
  Nothing -> []

filter'' :: (Int -> Boolean) -> Array Int -> Array Int
filter'' f arr = case uncons arr of
  Just {head: x, tail} | f x -> x : filter'' f tail
  Just {tail} -> filter'' f tail
  Nothing -> []

main :: Effect Unit
main = do
  log $ show $ filter' (\x -> x > 2) (1..10)
