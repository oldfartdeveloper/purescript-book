module MyWork where

import Prelude
import Math (pi, pow)
import Data.Int as Int
import Data.Array (head, length, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Picture (Shape(..), Point(..), showPoint, showShape)

-- gcd :: Int -> Int -> Int
-- gcd n 0 = n
-- gcd 0 m = m
-- gcd n m = if n > m
--             then gcd (n - m) m
--             else gcd n (m - n)
fromString :: String -> Boolean
fromString "true" = true

fromString _ = false

toString :: Boolean -> String
toString true = "true"

toString false = "false"

--| factorial
fact :: Int -> Int
fact 0 = 1

fact n = n * fact (n - 1)

--| I'm not implmenting the correct code; above certain values, it doesn't calculate accurately
pascalsRule :: Int -> Int -> Int
pascalsRule _ 0 = 1

pascalsRule n r =
  let
    factToNum :: Int -> Number
    factToNum n' = Int.toNumber $ fact n'
  in
    Int.round $ ((factToNum n) / ((factToNum r) * factToNum (n - r)))

type Address
  = { street :: String, city :: String }

type Person
  = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity p1 p2 = p1.address.city == p2.address.city

-- | Most general type for this function
sameCity' :: { address :: { city :: String } } -> { address :: { city :: String } } -> Boolean
sameCity' { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

-- | Most general type for this function
livesInLA' :: { address :: { city :: String } } -> Boolean
livesInLA' { address: { city: c } } = c == "Los Angeles"

fromSingleton :: ∀ a. a -> Array a -> a
fromSingleton def arr = case uncons arr of
  Just { head: x, tail: xs } -> if 0 == length xs then x else def
  Nothing -> def

fromSingleton' :: ∀ a. a -> Array a -> a
fromSingleton' def arr
  | 1 == length arr = fromMaybe def $ head arr
  | otherwise = def

fromSingleton'' :: ∀ a. a -> Array a -> a
fromSingleton'' def [] = def
fromSingleton'' _ [ x ] = x
fromSingleton'' def _ = def


origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

exampleCircle :: Shape
exampleCircle = Circle c r
  where
    c :: Point
    c = origin

    r :: Number
    r = 10.0

doubledPoint :: Point -> Point
doubledPoint (Point { x, y }) = Point { x: (2.0 * x), y: (2.0 * y) }

showOrigin :: String
showOrigin = showPoint origin

showExampleCircle :: String
showExampleCircle = showShape exampleCircle

doubleShape :: Shape -> Shape
doubleShape (Circle c r) = Circle c (2.0 * r)
doubleShape (Rectangle c w h) = Rectangle c (2.0 * w) (2.0 * h)
doubleShape (Line start end) = Line origin (doubledPoint end)
doubleShape (Text p text) = Text origin text

showDoubleShape :: Shape -> String
showDoubleShape shape = showShape (doubleShape shape)

extractText :: Shape -> Maybe String
extractText (Text p text) = Just text
extractText _ = Nothing

area :: Shape -> Number
area (Circle c r) = pi * (r `pow` 2.0)
area (Rectangle c w h) = w * h
area (Line start end) = 0.0
area (Text p text) = 0.0
