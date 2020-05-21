module Test.WorkFile where

import Prelude
-- import Data.AddressBook
--   ( Address
--   , PhoneNumber
--   , address
--   )
-- import Data.AddressBook.Validation
--   ( Errors
--   , arrayNonEmpty
--   , matches
--   , nonEmpty
--   , validateAddress
--   , validatePhoneNumber
--   )
-- import Data.Either (Either(..))
import Data.Maybe (Maybe, applyMaybe)

-- import Data.Traversable (traverse)
-- import Data.Validation.Semigroup (V)
-- import Data.String.Regex (Regex(), regex)
-- import Data.String.Regex.Flags (noFlags)
-- import Effect (Effect)
-- import Effect.Console (logShow)
-- import Partial.Unsafe (unsafePartial)
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe i j = (+) <$> i <*> j

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe i j = (-) <$> i <*> j

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe i j = (*) <$> i <*> j

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe i j = (/) <$> i <*> j

addApply :: ∀ a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
addApply i j = (+) <$> i <*> j

-- lift3 ::
--   forall a b c d f.
--   Apply f =>
--   (a -> b -> c -> d) ->
--   f a ->
--   f b ->
--   f c ->
--   f d
-- lift3 f x y z = f <$> x <*> y <*> z
-- combineMaybe :: ∀ a f. Applicative f => Maybe (f a) -> f (Maybe a)
-- combineMaybe (Just x) = map Just x
-- combineMaybe _ = pure Nothing
-- {-| Exercise Group 2 -}
-- validateAddressRegex :: Address -> V Errors Address
-- validateAddressRegex a =
--   address <$> (nonEmpty "Street" a.street *> pure a.street)
--     <*> (nonEmpty "City" a.city *> pure a.city)
--     <*> (matches "State" stateAbbreviationRegex a.state *> pure a.state)
-- stateAbbreviationRegex :: Regex
-- stateAbbreviationRegex =
--   unsafePartial case regex "^[A-Z]{2}$" noFlags of
--     Right r -> r
-- validateAddressRegex' :: Address -> V Errors Address
-- validateAddressRegex' a =
--   address <$> (nonEmpty "Street" a.street *> pure a.street)
--     <*> (matches "City" notOnlyWhitespaceRegex a.city *> pure a.city)
--     <*> (matches "State" stateAbbreviationRegex a.state *> pure a.state)
-- notOnlyWhitespaceRegex :: Regex
-- notOnlyWhitespaceRegex =
--   unsafePartial case regex "[0-9A-Za-z]" noFlags of
--     Right r -> r
-- {-| Exercise Group 3 -}
-- {-| Exercise 1: Write a Traversable instance for a binary tree data structure.
-- Acknowledgeent: this solution presented by hrk at
-- https://discourse.purescript.org/t/purescript-by-example-tree-traversal-question/981/2
-- Run as follows:
-- spago repl
-- > import Test.Solutions
-- > runTraversableExercise
-- 1
-- 2
-- 3
-- 2
-- 1
-- 3
-- 1
-- 3
-- 2
-- unit
-- > 
-- The numbers above illustrate different traversal orders.
-- TODO: Incorporate this in unit tests.
-- -}
-- data Tree a
--   = Leaf
--   | Branch (Tree a) a (Tree a)
-- sampleTree :: Tree Int
-- sampleTree = Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
-- traverseInOrder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
-- traverseInOrder _ Leaf = pure Leaf
-- -- first evaluate left branch, then node, then right branch
-- traverseInOrder f (Branch l x r) = Branch <$> traverseInOrder f l <*> f x <*> traverseInOrder f r
-- traversePreOrder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
-- traversePreOrder _ Leaf = pure Leaf
-- -- first evaluate node, then left branch and then right branch. Notice the lambda function shuffles the arguments where they need to be
-- traversePreOrder f (Branch l x r) = (\x' l' r' -> Branch l' x' r') <$> f x <*> traversePreOrder f l <*> traversePreOrder f r
-- traversePostOrder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
-- traversePostOrder _ Leaf = pure Leaf
-- traversePostOrder f (Branch l x r) = (\l' r' x' -> Branch l' x' r') <$> traversePostOrder f l <*> traversePostOrder f r <*> f x
-- runTraversableExercise :: Effect Unit
-- runTraversableExercise = do
--   t1 <- traverseInOrder logShow sampleTree -- 1 2 3
--   t2 <- traversePreOrder logShow sampleTree -- 2 1 3
--   t3 <- traversePostOrder logShow sampleTree -- 1 3 2  
--   pure unit
-- {-| Exercise 2: Test when 'address' field is a Maybe -}
-- type Person'
--   = { firstName :: String
--     , lastName :: String
--     , homeAddress :: Maybe Address
--     , phones :: Array PhoneNumber
--     }
-- person' :: String -> String -> Maybe Address -> Array PhoneNumber -> Person'
-- person' firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }
-- validatePersonWithMaybeAddress :: Person' -> V Errors Person'
-- validatePersonWithMaybeAddress p =
--   person' <$> (nonEmpty "First Name" p.firstName *> pure p.firstName)
--     <*> (nonEmpty "Last Name" p.lastName *> pure p.lastName)
--     <*> (traverse validateAddress p.homeAddress *> pure p.homeAddress)
--     <*> (arrayNonEmpty "Phone Numbers" p.phones *> traverse validatePhoneNumber p.phones)
-- {-| Exercise 3: Try to write sequence in terms of traverse and vice-versa
-- TODO
-- -}
