module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly, pending')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- import Test.Nodes (spec) as Nodes
import Test.Formatting (spec) as Formatting
import Test.JsonSerialize (spec) as JsonSerialize


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Formatting"
    Formatting.spec
  describe "Serialization"
    JsonSerialize.spec
