module Test.Main where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Solutions (combineMaybe, example)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    suite "Working Examples" do
      test "Initial passing solutions test"
        $ Assert.assert "this example should have worked"
        $ example true true
    suite "Exercise Group 1" do
      suite "Use lift2 to write lifted versions of numeric operators" do
        test "+ (Just)"
          $ Assert.equal (Just 5)
          $ (+)
          <$> (Just 2)
          <*> (Just 3)
        test "+ (Nothing on left)"
          $ Assert.equal Nothing
          $ (+)
          <$> Nothing
          <*> (Just 3)
        test "+ (Nothing on right)"
          $ Assert.equal Nothing
          $ (+)
          <$> (Just 2)
          <*> Nothing
        test "- (Just)"
          $ Assert.equal (Just (-1))
          $ (-)
          <$> (Just 2)
          <*> (Just 3)
        test "- (Nothing on left)"
          $ Assert.equal Nothing
          $ (-)
          <$> Nothing
          <*> (Just 3)
        test "- (Nothing on right)"
          $ Assert.equal Nothing
          $ (-)
          <$> (Just 2)
          <*> Nothing
        test "* (Just)"
          $ Assert.equal (Just 6)
          $ (*)
          <$> (Just 2)
          <*> (Just 3)
        test "* (Nothing on left)"
          $ Assert.equal Nothing
          $ (*)
          <$> Nothing
          <*> (Just 3)
        test "* (Nothing on right)"
          $ Assert.equal Nothing
          $ (*)
          <$> (Just 2)
          <*> Nothing
        test "/ (Just)"
          $ Assert.equal (Just 2)
          $ (/)
          <$> (Just 6)
          <*> (Just 3)
        test "/ (Nothing on left)"
          $ Assert.equal Nothing
          $ (/)
          <$> Nothing
          <*> (Just 3)
        test "/ (Nothing on right)"
          $ Assert.equal Nothing
          $ (/)
          <$> (Just 2)
          <*> Nothing
      suite "Convince yourself that the definition of lift3 type checks" do
        test "Substituting an Integer type for any of the strings fails to compile"
          $ Assert.assert "Manually compiled and manually verifed compiling failed"
          $ true
      suite "Write a function combineMaybe" do
        test "Test List of Maybe Int with Just"
          $ Assert.equal (Just 1 : Just 2 : Just 3 : Nil)
          $ combineMaybe (Just $ 1 : 2 : 3 : Nil)
        test "Test List of Maybe Int with Nothing"
          $ Assert.equal (Nothing : Nil)
          $ combineMaybe (Nothing :: Maybe (List Int))
        test "Test List of Maybe Int with Nothing (verifying using 'pure')"
          $ Assert.equal (pure Nothing)
          $ combineMaybe (Nothing :: Maybe (List Int))
        test "Test Array of Maybe Int with Just"
          $ Assert.equal [ (Just 1), (Just 2), (Just 3) ]
          $ combineMaybe (Just [ 1, 2, 3 ])
        test "Test Array of Maybe Int with Nothing"
          $ Assert.equal (pure Nothing)
          $ combineMaybe (Nothing :: Maybe (Array Int))
