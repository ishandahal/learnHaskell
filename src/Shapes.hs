module Shapes
  ( Shape,
    Car (..),
    Point,
    surface,
    baseCircle,
    baseRectangle,
  )
where

import Data.Time (DayOfWeek (Sunday))

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle height width = Rectangle (Point 0 0) (Point height width)

-- Record syntax
data Car = Car
  { make :: String,
    model :: String,
    color :: String,
    year :: Int
  }
  deriving (Show)

-- type constructor
data Vector a b c = Vector a b c deriving (Show)

vectorSum :: (Ord a, Num a) => Vector a a a -> a
vectorSum (Vector x y z) = x + y + z

vectorProduct :: Vector Float Float Float -> Float
vectorProduct (Vector x y z) = x * y * z

-- Derived Instances
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Ord, Eq, Enum, Bounded, Show)
