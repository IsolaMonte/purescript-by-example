module Main where

import Prelude
import Data.EuclideanRing (div)

-- 5. Pattern matching
-- This chapter will introduce two new concepts: algebraic data types, and pattern matching.
-- We will also briefly cover an interesting feature of the PureScript type system: row polymorphism.

-- Eucledian algorithm
gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 n = n
gcd' n m | n > m     = gcd' (n - m) m
         | otherwise = gcd' n (m - n)


-- EXERCISES
{-
1. (Easy) Write the factorial function using pattern matching. Hint. Consider the two cases zero and non-zero inputs.
-}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

{-
2. (Medium) Look up Pascal's Rule for computing binomial coefficients.
Use it to write a function which computes binomial coefficients using pattern matching.
-}
pascal :: Int -> Int -> Int
pascal 1 k = 1
pascal n k = factorial n `div` ( factorial (n - k) * factorial k )
-- `div` can be used instead of / since all numbers will divide "nicely"


