module Main where

import Prelude

import Data.EuclideanRing (div)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (foldl, sum)

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


-- 5.6 Array patterns
isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

-- 5.7 Record Patterns and Row Polymorphism
showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

-- “takes any record with first and last fields which are Strings and any other fields, and returns a String”.
showPerson' :: forall r. { first :: String, last :: String | r } -> String
showPerson' { first: x, last: y } = y <> ", " <> x


--5.8 Nested Patterns
--This code combines two record patterns:
type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false


--5.9 Named Patterns
sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

-- EXERCISES
{-
(Easy) Write a function sameCity which uses record patterns to test whether two Person records belong to the same city.
-}
sameCity :: Person -> Person -> Boolean
sameCity p1 p2 = if (p1.address.city == p2.address.city)
                    then true
                 else false

sameCity' :: Person -> Person -> Boolean
sameCity' { address: { city: a } } { address: { city: b } } = if (a == b) then true else false

{-
(Medium) What is the most general type of the sameCity function,
taking into account row polymorphism? What about the livesInLA function defined above?
-}
{-
emma = { name: "Emma", address: { street: "Sveavägen", city: "Oslo" } }
erika = { name: "Erika", address: { street: "Drottinggatan", city: "Stockholm" } }
elin = { name: "Elin", address: { street: "Kungsgatan", city: "Stockholm" } }
-}
sameCity'' :: forall r. { address :: Address | r } -> { address :: Address | r } -> Boolean
sameCity'' { address: { city: a } } { address: { city: b } } = if (a == b) then true else false

{-
(Medium) Write a function fromSingleton which uses an array literal pattern to extract the sole member of a singleton array.
If the array is not a singleton, your function should return a provided default value. Your function should have type forall a. a -> Array a -> a
-}
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a


--5.10 Case Expressions
lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
           0 -> xs
           _ -> lzs (unsafePartial tail xs)


--5.11 Pattern Match Failures and Partial Functions
partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true


--5.12 Algebraic Data Types

