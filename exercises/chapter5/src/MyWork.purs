module MyWork (fromSingleton, exampleCircle) where

import Prelude
import Data.Int as Int
import Data.Array (length, uncons)
import Data.Maybe (Maybe(..))
import Data.Picture (Shape(..), Point(..))
-- import Data.Show (show) as Show

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
fromSingleton default arr = case uncons arr of
  Just { head: x, tail: xs } -> if 0 == length xs then x else default
  Nothing -> default

-- Haven't been able to figure out how to apply array literal pattern here.
-- fromSingleton :: ∀ a. a -> Array a -> a
-- fromSingleton default arr
--   | _ [] = default
--   | _ arr = if 0 == length xs then x else default

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

-- doubleShape :: Shape -> Shape
-- doubleShape (Circle c r) = Circle c (2.0 * r)
-- doubleShape (Rectangle c w h) = Rectangle c (2.0 * w) (2.0 * h)
-- doubleShape (Line start end) =
--   where
--     start :: Point
--     start = origin

--     end :: Point
--     end = Point { x: (2.0 * end.x), y: (2.0 * end.y) }
-- doubleShape (Text p text) = Text origin
