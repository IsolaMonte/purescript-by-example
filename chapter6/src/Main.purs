module Main where

import Prelude
import Data.Foldable

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


--6.4 Common Type Classes

--EXERCISES
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
    eq (Complex x1) (Complex x2) = x1.real == x2.real && x1.imaginary == x2.imagniary


--6.5 Type Annotations


--6.6 Overlapping Instances


-- EXERCISES

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
instance nonEmptySemigroup :: Semigroup (Array a) => Semigroup (NonEmpty a) where
    append (NonEmpty x xs) (NonEmpty y ys) = ?_

{-
3. (Medium) Write a Functor instance for NonEmpty.
-}

{-
4. (Medium) Given any type a with an instance of Ord,
we can add a new “infinite” value which is greater than any other value:
-}
data Extended a = Finite a | Infinite
--Write an Ord instance for Extended a which reuses the Ord instance for a.

{-
5. (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays.
-}

{-
6. (Difficult) Given an type constructor f which defines an ordered container (and so has a Foldable instance),
we can create a new container type which includes an extra element at the front:
-}
data OneMore f a = OneMore a (f a)
--The container OneMore f is also has an ordering,
--where the new element comes before any element of f. Write a Foldable instance for OneMore f:

--instance foldableOneMore :: Foldable f => Foldable (OneMore f) where