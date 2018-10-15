module Main where

import Prelude

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