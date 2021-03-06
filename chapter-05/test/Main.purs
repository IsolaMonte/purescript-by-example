module Test.Main where

import Prelude (Unit, discard)

import Effect
import Effect.Console (log)
import Data.Picture (Point(..), Shape(..), Picture, bounds, showBounds)

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

main :: Effect Unit
main = log (showBounds (bounds picture))