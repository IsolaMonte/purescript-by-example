module Main where

import Prelude (div, otherwise, show, (*), (-), (<=), (<>), (==), (>))
import Data.Picture
import Data.Array.Partial (tail)
import Data.Foldable (sum)
import Partial.Unsafe (unsafePartial)
import Math (pi, pow)


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
{-
An algebraic data type is introduced using the data keyword, followed by the name of the new type and any type arguments.
The type’s constructors are defined after the equals symbol, and are separated by pipe characters (|).
-}

-- “a value of type Maybe a is either Nothing, or Just a value of type a”.
data Maybe a = Nothing | Just a


--5.13 Using ADTs
exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 0.0 }
    p2 :: Point
    p2 = Point { x: 100.0, y: 50.0 }

-- Pattern match on constructor to get the values out!
showPoint :: Point -> String
showPoint (Point { x: x, y: y }) =
  "(" <> show x <> ", " <> show y <> ")"


--5.14 Record Puns
origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0


-- EXERCISES
{-
(Easy) Construct a value of type Shape which represents a circle centered at the origin with radius 10.0.
-}
drawCircle :: Shape
drawCircle = Circle center radius
  where
    center = origin
    radius = 10.0

--instance showPoint' :: Show Point where
--    show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

--instance showShape :: Show Shape where
--    show (Circle c r) = "Circle origin: " <> show c <> ", with radius: " <> show r
--    show _ = "Nothing"

{-
(Medium) Write a function from Shapes to Shapes, which scales its argument by a factor of 2.0, center the origin.
-}
scaleUp :: Shape -> Shape
scaleUp (Circle c r) = Circle center radius
  where
    center = origin
    radius = 2.0 * r
scaleUp _ = drawCircle


{-
(Medium) Write a function which extracts the text from a Shape. It should return Maybe String,
and use the Nothing constructor if the input is not constructed using Text.
-}
getText :: Shape -> Maybe String
getText (Text p s) = Just s
getText _ = Nothing

--instance showMaybe :: Show a => Show (Maybe a) where
--  show (Just x) = "(Just " <> show x <> ")"
--  show Nothing  = "Nothing"

-- EXERCISES
{-
1. (Medium) Extend the vector graphics library with a new operation area which computes the area of a Shape.
For the purposes of this exercise, the area of a piece of text is assumed to be zero.
-}
area :: Shape -> Number
area (Text p s) = 0.0
area (Circle o r) = pi * (pow r 2.0)
area (Rectangle o n1 n2) = n1 * n2
area (Line p1 p2) = 0.0

{-
2. (Difficult) Extend the Shape type with a new data constructor Clipped,
which clips another Picture to a rectangle.
Extend the shapeBounds function to compute the bounds of a clipped picture.
Note that this makes Shape into a recursive data type.
-}