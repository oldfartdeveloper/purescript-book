module FileOperations where

import Prelude

import Data.Foldable (foldl)
import Data.Int (rem, quot)
-- import Data.Ord (min)
-- import Data.Pair (Pair(..), fst, snd, (~))
import Data.Path (Path(), filename, isDirectory, ls, root, size)
import Data.Array (concatMap, cons, filter, head, length, tail, (:), (..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)
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
          
largestSmallest :: Path -> Array (Tuple String Int)
largestSmallest path = largestSmallestPaths (allFiles path) where
  largestSmallestPaths :: Array Path -> Array (Tuple String Int)
  largestSmallestPaths paths =
    [outlier (\i j -> i > j) 0 paths, outlier (\i j -> i < j) maxSigned32BitInt paths] where
      outlier :: (Int -> Int -> Boolean) -> Int -> Array Path -> Tuple String Int
      outlier criteria startValue paths' =
        foldl (\acc p' -> (case size p' of
                            Just n -> 
                              if criteria n $ snd acc then Tuple (filename p') n else acc
                            Nothing -> acc
                          )
              ) (Tuple "" startValue) paths' 

allSizes :: Array Path -> Array (Tuple String Int)
allSizes paths =
  map (\p -> case size p of
                Just n -> Tuple (filename p) n
                Nothing -> Tuple (filename p) 0
      ) paths

whereIs :: String -> Maybe Path
whereIs fileName =
  Just root
  