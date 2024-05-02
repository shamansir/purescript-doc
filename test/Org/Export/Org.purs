module Test.Org.Export.Org where

import Prelude

import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
-- import Data.Text.Doc as D
import Data.Text.Doc as D
import Data.Text.Format.Org.Types (Check(..))
import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Render as R

import Yoga.JSON (read_, write, writeJSON)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)



spec :: Spec Unit
spec = do

  describe "export to ORG" $ do

    {-
    it "works with the empty .org files" $ do
        (read_ $ write Org.empty) `shouldEqual` (Just Org.empty)
    -}

    it "works with the syntax sample" $ do
        orgTest01 <- liftEffect $ readTextFile UTF8 "./test/examples/org-test-01.org"
        (D.render { break : D.All, indent : D.Spaces 4 } $ R.layout Org.empty)
            `shouldEqual` orgTest01
