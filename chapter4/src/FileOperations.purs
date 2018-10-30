module Main where

import Prelude (bind, discard, map, mod, pure, ($), (&&), (*), (+), (-), (/), (<), (<<<), (<>), (==), (>), (>=))
import Control.MonadZero (guard)
import Data.Array (head, uncons, length, filter, concatMap, (:), (..))
import Data.Array.Partial as Partial
import Data.Foldable (product, foldl, foldr, null)
import Data.Maybe (Maybe(..))
import Data.Path (size, filename, root, isDirectory, Path, ls)
import Data.HeytingAlgebra (not)
import Partial.Unsafe (unsafePartial)

-- 4. Recursion, Maps And Folds

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- EXCERCISES

{-
1. (Easy) Write a recursive function which returns true if and only if its input is an even integer.
-}
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

isEven' :: Int -> Boolean
isEven' n = n `mod` 2 == 0

length' :: forall a. Array a -> Int
length' arr =
    if null arr
        then 0
        else 1 + length' (unsafePartial Partial.tail arr)

{-
2. (Medium) Write a recursive function which counts the number of even integers in an array.
Hint: the function unsafePartial head (where head is also imported from Data.Array.Partial) can be used to find the first element in a non-empty array.
-}
countEven :: Array Int -> Int
countEven l =
  case uncons l of
    Just { head, tail } ->
      let
        s =
         if isEven head
         then 1
         else 0
      in
        s + countEven tail
    Nothing -> 0

countEven' :: Array Int -> Int
countEven' = length <<< filter isEven

-- EXCERCISES

{-
1. (Easy) Use the map or <$> function to write a function which calculates the squares of an array of numbers.
-}
mapSquare :: Array Int -> Array Int
mapSquare = map (\n -> n * n)

{-
2. (Easy) Use the filter function to write a function which removes the negative numbers from an array of numbers.
-}
removeNegatives :: Array Int -> Array Int
removeNegatives = filter (\n -> n > 0 )

{-
3. (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to the previous question to use your new operator.
Experiment with the precedence level and associativity of your operator in PSCi.
-}
infix 8 filter as <$?>

removeNegatives' :: Array Number -> Array Number
removeNegatives' arr = (\n -> n >= 0.0) <$?> arr


-- 4.9 Array Comprehensions
pairs' :: Int -> Array (Array Int)
pairs' n =
  concatMap (\i -> map (\j -> [i, j]) (1 .. n)) (1 .. n)

pairs'' :: Int -> Array (Array Int)
pairs'' n =
  concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)


-- 4.10 Do Notation
factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs'' n)

factors' :: Int -> Array (Array Int)
factors' n = filter (\xs -> product xs == n) $ do
    i <- 1 .. n
    j <- i .. n
    pure [i, j]

factorsWithGuard :: Int -> Array (Array Int)
factorsWithGuard n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n
    pure [i, j]


-- EXCERCISES

{-
1. (Easy) Use the factors function to define a function isPrime which tests if its integer argument is prime or not.
Prime numbers are positive, non-zero numbers that have exactly two factors -- no more, no less.
-}
isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

{-
2. (Medium) Write a function which uses do notation to find the cartesian product of two arrays,
i.e. the set of all pairs of elements a, b, where a is an element of the first array, and b is an element of the second.
Demonstrate that your function/method correctly returns:
{1, 2} × {3, 4} = {(1, 3), (1, 4), (2, 3), (2, 4)}
and, in contrast:
{3, 4} × {1, 2} = {(3, 1), (3, 2), (4, 1), (4, 2)}
-}

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct arr0 arr1 =
    concatMap (\i -> map (\j -> [i, j]) arr1) arr0

cartesianProduct' :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct' arrA arrB = do
  a <- arrA
  b <- arrB
  pure [a, b]


{-
3. (Medium) A Pythagorean triple is an array of numbers [a, b, c] such that a² + b² = c².
Use the guard function in an array comprehension to write a function triples which takes a number n and calculates all Pythagorean triples whose components are less than n.
Your function should have type Int -> Array (Array Int).
-}
triples :: Int -> Array (Array Int)
triples c = do
    a <- 1 .. c
    b <- a .. c
    guard (a * a + b * b == c * c)
    pure[a, b, c]


{-
4. (Difficult) Write a function factorizations which produces all factorizations of an integer n,
i.e. arrays of integers whose product is n.
Hint: for an integer greater than 1, break the problem down into two subproblems:
finding the first factor, and finding the remaining factors.
-}
factorizations :: Int -> Array (Array Int)
factorizations n = [n] : do
  x <- factors'' n
  guard $ x > 1 && x < n
  xs <- factorizations $ n / x
  pure $ x : xs

factors'' :: Int -> Array Int
factors'' n = do
  x <- 1 .. n
  guard $ (n `mod` x) == 0
  pure x


--4.14 Accumulators
reverse :: forall a. Array a -> Array a
reverse = reverse' []
  where
    reverse' acc [] = acc
    reverse' acc xs = reverse' (unsafePartial Partial.head xs : acc)
                               (unsafePartial Partial.tail xs)


-- 4.15 Prefer Folds to Explicit Recursion
reverseR :: forall a. Array a -> Array a
reverseR = foldr (\x xs -> xs <> [x]) []


-- EXCERCISES
{-
1. (Easy) Use foldl to test whether an array of boolean values are all true.
-}
isTrue :: Array Boolean -> Boolean
isTrue = foldl (\x y -> x && y) true

{-
2. (Medium) Characterize those arrays xs for which the function foldl (==) false xs returns true.
-}
--allTrue will returns true for isTrue [true, true, true]
allTrue :: Array Boolean -> Boolean
allTrue = foldl (==) false

{-
3. (Medium) Rewrite the following function in tail recursive form using an accumulator parameter:
-}
count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial Partial.head xs)
               then count p (unsafePartial Partial.tail xs) + 1
               else count p (unsafePartial Partial.tail xs)

countTailReq :: forall a. (a -> Boolean) -> Array a -> Int -> Int
countTailReq = countTailReq'
    where
        countTailReq' :: forall b. (b -> Boolean) -> Array b -> Int -> Int
        countTailReq' _ [] _ = 0
        countTailReq' p xs acc =
            if p (unsafePartial Partial.head xs)
                then countTailReq' p (unsafePartial Partial.tail xs) acc + 1
                else countTailReq' p (unsafePartial Partial.tail xs) acc

countTailReq'' :: forall a. (a -> Boolean) -> Array a -> Int
countTailReq'' p = count' 0
  where
    count' acc arr = case uncons arr of
      Nothing -> acc
      Just { head: x, tail: xs } -> if p x
        then count' (acc + 1) xs
        else count' acc xs

{-
4. (Medium) Write reverse in terms of foldl.
-}
reverseL :: forall a. Array a -> Array a
reverseL = foldl (\xs x -> [x] <> xs) []


--4.17 Listing All Files
allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

-- Great! Now let’s see if we can write this function using an array comprehension using do notation.
allFiles' :: Path -> Array Path
allFiles' file = file : do
    child <- ls file
    allFiles' child


-- EXCERCISES
{-
1. (Easy) Write a function onlyFiles which returns all files (not directories) in all subdirectories of a directory.
-}
onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

{-
2. (Medium) Write a fold to determine the largest and smallest files in the filesystem.
-}
--foldl :: forall a b f. Foldable f => (b -> a -> b) -> b -> f a -> b
smallestFile :: Path -> Maybe Path
smallestFile = foldl smallest Nothing <<< onlyFiles
  where
    smallest Nothing path = Just path
    smallest (Just acc) path = if size acc < size path
      then Just acc
      else Just path

largestFile :: Path -> Maybe Path
largestFile = foldl largest Nothing <<< onlyFiles
    where
      largest Nothing path = Just path
      largest (Just acc) path = if size acc > size path
        then Just acc
        else Just path

{-
3. (Difficult) Write a function whereIs to search for a file by name.
The function should return a value of type Maybe Path, indicating the directory containing the file, if it exists.
It should behave as follows:
 > whereIs "/bin/ls"
 Just (/bin/)
 > whereIs "/bin/cat"
 Nothing
Hint: Try to write this function as an array comprehension using do notation.
-}
whereIs :: String -> Maybe Path
whereIs file = head $ do
  path <- allFiles' root
  child <- ls path
  guard $ filename child == file
  pure path