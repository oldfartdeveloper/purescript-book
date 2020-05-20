module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    suite "Verifying unit test environment" do
      test "Initial passing test"
        $ Assert.equal true true
