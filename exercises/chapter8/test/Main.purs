module Test.Main where

import Prelude
import Control.Monad.Free (Free)
import Data.List
  ( List(..)
  , foldM
  , (:)
  )
import Data.Maybe (Maybe(..))
import Effect (Effect)
{- | Enable your own code to be tested by removing this comment
import MySolutions
  (
  )
-}
-- import Official.Solutions
--   (
--   )
import Test.Examples (safeDivide)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    runChapterExamples
    suite "Verifying unit test environment" do
      test "Initial passing test"
        $ Assert.equal true true

runChapterExamples :: Free TestF Unit
runChapterExamples =
  suite "Chapter Examples" do
    suite "Testing foldM and safeDivide" do
      test "[5, 2, 2] has a Just answer"
        $ Assert.equal (Just 5)
        $ foldM safeDivide 100 (5 : 2 : 2 : Nil)
      test "[5, 0, 2] has a Nothing answer"
        $ Assert.equal (Nothing)
        $ foldM safeDivide 100 (5 : 0 : 2 : Nil)
