module FileOperations where

import Prelude

import Data.Foldable (foldl)
import Data.Int (rem, quot)
-- import Data.Ord (min)
-- import Data.Pair (Pair(..), fst, snd, (~))
import Data.Path (Path(), isDirectory, ls, size)
import Data.Array (concatMap, cons, filter, head, length, tail, (:), (..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

isEven :: Int -> Boolean
isEven n =
  case n of
    0 -> true
    1 -> false
    _ -> isEven $ n - 2

oneIfEven :: Int -> Int
oneIfEven n =
  if isEven n then 1 else 0

evenCount :: Array Int -> Int
evenCount ints = evenCount' ints 0
  where
    evenCount' :: Array Int -> Int -> Int
    evenCount' [] count = count
    evenCount' ints' count =
      evenCount' (fromMaybe [] (tail ints')) $ add count $ maybe 0 oneIfEven $ head ints'

squared :: Array Number -> Array Number
squared arr = map (\n -> n * n) arr

infix 4 filter as <$?>

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = (\n -> n >= 0.0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- (n - 1) .. 1 
  j <- 2 .. i
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n =
  eq 1 $ length $ factors n

cartesianProduct :: ∀ a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct left right = do
  a_ <- left
  b_ <- right
  pure $ Tuple a_ b_

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  pure [i, j, k]

factor :: Int -> Array Int
factor n = do
  i <- 2 .. (n - 1)
  j <- i .. (n - 1)
  guard $ i * j == n
  pure j

-- | Provide the prime numbers that, multiplied together, make the argument.
factorizations :: Int -> Array Int
factorizations n = factorizations' 2 n []
 where
  factorizations' :: Int -> Int -> Array Int -> Array Int
  factorizations' _ 1 result = result
  factorizations' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
    if remainder == 0 then
      factorizations' (divisor) (quot dividend divisor) (cons divisor result)
      else factorizations' (divisor + 1) dividend result

allTrue :: Array Boolean -> Boolean
allTrue bools = foldl (\acc bool -> acc && bool) true bools 

-- | > mystery [false, true, true, true, true, true, true]
-- | > true
-- | > 
-- | > mystery [true, false, true, true, true, true, true]
-- | > true
-- | > 
-- | > mystery [true, false, true, true, true, true, true, false]
-- | > false
exclusiveOrThenTrue :: Array Boolean -> Boolean
exclusiveOrThenTrue bools = foldl (==) false bools

--| From the book:
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | The fib routine in tail recursive form
fib' :: Int -> Int
fib' n = fib'' n 0 0 1 
  where
    fib'' :: Int -> Int -> Int -> Int -> Int
    fib'' limit count n1 n2 =
      if limit == count then
        n1 + n2
      else
        fib'' limit (count + 1) (n1 + n2) n1

reverse' :: ∀ a. Array a -> Array a
reverse' = foldl (\xs x -> [x] <> xs) []

-- Section for : A Virtual Filesystem exercise

onlyFiles :: Path -> Array Path
onlyFiles p =
  filter (\p' -> not $ isDirectory p') $ allFiles p

maxSigned32BitInt :: Int
maxSigned32BitInt = 2147483647

-- largestSmallestIn :: Path -> Pair Path Path
-- largestSmallestIn p = lsi (onlyFiles p) (Pair maxSigned32BitInt -1)
--   where
--     lsi :: Array Path -> Pair Path Path -> Pair Path Path
--     lsi paths pair =
--       foldl (\p' pair' -> (fst pair') `min` p'.size ~ (snd pair') `max` p'.size) pair paths
                  
-- largestSmallestIn :: Path -> Pair Path Path
-- largestSmallestIn p = lsi (onlyFiles p) (Pair maxSigned32BitInt -1)
--   where
--     lsi :: Array Path -> Pair Tuple(Path, Int) ~ Tuple(Path, Int) -> Pair (Int ~ Int) -> Pair Path Path
--     lsi paths ((lPath, lSize) ~ (rPath, rSize)) (existingMin ~ existingMax) =
--       let
--         sizeCompare :: Boolean -> Int -> Path -> Int -> Path -> Tuple Int Path 
--         sizeCompare isWantSmaller existingSize existingPath compareSize comparePath =
--         if isWantSmaller && existingSize > compareSize then
--           Tuple compareSize comparePath
--         else if existingSize < compareSize then
--           Tuple compareSize comparePath
--         else existingSize existingPath
--       in
--         foldl (\p' pair' -> sizeCompare false fst $ snd pair pair paths
          
smallestSize :: Path -> Int
smallestSize p =
  foldl (\acc p' -> min acc (case size p' of
                              Just n -> n
                              Nothing -> acc
                            )
        ) maxSigned32BitInt (onlyFiles p) 

allSizes :: Array Path -> Array Int
allSizes paths =
  map (\p -> case size p of
                Just n -> n
                Nothing -> -1
      ) paths
