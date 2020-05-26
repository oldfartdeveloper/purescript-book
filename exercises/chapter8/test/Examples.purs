module Test.Examples where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

foldM ::
  forall m a b.
  Monad m =>
  (a -> b -> m a) ->
  a ->
  List b ->
  m a
foldM _ a Nil = pure a

foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing

safeDivide a b = Just (a / b)
