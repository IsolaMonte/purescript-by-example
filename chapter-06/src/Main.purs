module Main where

import Prelude (class Eq, class Functor, class Ord, class Semigroup, class Show, Ordering(..), map, show, (&&), (<), (<>), (==), (>))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)

--6. Type Classes

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

-- EXERCISES
{-
(Easy) Use the showShape function from the previous chapter to define a Show instance for the Shape type.
-}

instance showPoint :: Show Point where
  show (Point { x, y }) =  "(" <> show x <> ", " <> show y <> ")"

instance showShape :: Show Shape where
  show (Circle c r) =
      "Circle [center: " <> show c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
      "Rectangle [center: " <> show c <> ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line start end) =
      "Line [start: " <> show start <> ", end: " <> show end <> "]"
  show (Text loc text) =
      "Text [location: " <> show loc <> ", text: " <> show text <> "]"


--EXERCISES: 6.4 Common Type Classes
{-
1.(Easy) The following newtype represents a complex number:
Define Show and Eq instances for Complex.
-}
newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex x) = "Real: " <> show x.real <> ", Imaginary: " <> show x.imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex x1) (Complex x2) = x1.real == x2.real && x1.imaginary == x2.imaginary


-- EXERCISES: 6.7 Instance Dependencies
{-
1. (Easy) The following declaration defines a type of non-empty arrays of elements of type a.
Write an Eq instance for the type NonEmpty a which reuses the instances for Eq a and Eq (Array a).
-}

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show ([x] <> xs)

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

{-
2. (Medium) Write a Semigroup instance for NonEmpty a by reusing the Semigroup instance for Array.
-}
instance semigroupNonEmpty :: Semigroup (Array a) => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

{-
3. (Medium) Write a Functor instance for NonEmpty.
-}
instance functorNonEmpty  :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

{-
4. (Medium) Given any type a with an instance of Ord,
we can add a new “infinite” value which is greater than any other value:
-}
data Extended a = Finite a | Infinite
--Write an Ord instance for Extended a which reuses the Ord instance for a.

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite a) Infinite = LT
  compare Infinite (Finite b) = GT
  compare Infinite Infinite = EQ
  compare (Finite a) (Finite b) = if a > b then GT
                                  else
                                      if a < b then LT
                                      else EQ

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite a) Infinite = false
  eq Infinite (Finite b) = false
  eq Infinite Infinite = true
  eq (Finite a) (Finite b) = a == b

{-
5. (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays.
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
-}
instance nonEmptyFoldable :: Foldable Array => Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = foldl f acc ([x] <> xs)
  foldr f acc (NonEmpty x xs) = foldr f acc ([x] <> xs)
  foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)

{-
6. (Difficult) Given a type constructor f which defines an ordered container (and so has a Foldable instance),
we can create a new container type which includes an extra element at the front:
-}
data OneMore f a = OneMore a (f a)
{- The container OneMore f also has an ordering,
where the new element comes before any element of f. Write a Foldable instance for OneMore f: -}

--instance foldableOneMore :: Foldable f => Foldable (OneMore f) where