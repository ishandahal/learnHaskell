module Shapes
  ( Shape,
    Car (..),
    Point,
    surface,
    baseCircle,
    baseRectangle,
  )
where

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
