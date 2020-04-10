module MyWork (fromSingleton) where

import Prelude
import Data.Int as Int
import Data.Array (length, uncons)
import Data.Maybe (Maybe(..))

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
