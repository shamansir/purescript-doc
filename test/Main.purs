module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly, pending')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- import Test.Nodes (spec) as Nodes
import Test.Formatting.Doc (spec) as FormattingDoc
import Test.Formatting.Blessed (spec) as FormattingBlessed
import Test.Org.Export.Json (spec) as OrgToJson
import Test.Org.Export.Org (spec) as OrgToOrg


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Formatting : Doc" $ do
    FormattingDoc.spec
  describe "Formatting : Blessed" $ do
    FormattingBlessed.spec
  describe "Org : to JSON"
    OrgToJson.spec
  describe "Org : to Org"
    OrgToOrg.spec
