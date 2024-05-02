module Test.Org.Export.Json where

import Prelude

import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
-- import Data.Text.Doc as D

import Data.Text.Format.Org.Types (Check(..))
import Data.Text.Format.Org.Construct as Org

import Yoga.JSON (read_, write, writeJSON)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)



spec :: Spec Unit
spec = do

  describe "JSON serialization" $ do

    it "properly encodes variants" $ do
        (writeJSON Check) `shouldNotEqual` (writeJSON Uncheck)
        (writeJSON Check) `shouldNotEqual` (writeJSON Halfcheck)
        (writeJSON Uncheck) `shouldNotEqual` (writeJSON Halfcheck)
        (read_ $ write Check) `shouldEqual` (Just Check)
        (read_ $ write Uncheck) `shouldEqual` (Just Uncheck)
        (read_ $ write Halfcheck) `shouldEqual` (Just Halfcheck)

    {-
    it "works with the empty .org files" $ do
        (read_ $ write Org.empty) `shouldEqual` (Just Org.empty)
    -}

    it "works with the empty JSON" $ do
        emptySample <- liftEffect $ readTextFile UTF8 "./test/examples/org-empty.json"
        (writeJSON Org.empty) `shouldEqual` emptySample
