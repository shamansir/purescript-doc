module Test.Org.Export.Org where

import Prelude

import Effect.Class (liftEffect, class MonadEffect)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Text.Doc as D
import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Render as R

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Org.Test.Test01 as Test01
import Org.Test.Test02a as Test02a
import Org.Test.Test02b as Test02b
import Org.Test.Test03a as Test03a
import Org.Test.Test03b as Test03b
import Org.Test.Test03c as Test03c
import Org.Test.Test03d as Test03d
import Org.Test.Test04a as Test04a
import Org.Test.Test04b as Test04b
import Org.Test.Test04c as Test04c
import Org.Test.Test04g as Test04g
import Org.Test.Test04e as Test04e


spec :: Spec Unit
spec = do

  describe "export to ORG" $ do

    {-
    it "works with the empty .org files" $ do
        (read_ $ write Org.empty) `shouldEqual` (Just Org.empty)
    -}

    it "01. works with the syntax sample" $
        qtest "01-empty" $ Test01.test

    it "02. works with the meta sample (a)" $
        qtest "02a-meta" $ Test02a.test

    it "02. works with the meta sample (b)" $
        qtest "02b-meta-special" $ Test02b.test

    it "03. works with basic headings and levels (a)" $
        qtest "03a-headings-with-no-content" $ Test03a.test

    it "03. works with heading with some content (b)" $
        qtest "03b-headings-with-content" $ Test03b.test

    it "03. works with headings with planning (c)" $
        qtest "03c-headings-with-planning" $ Test03c.test

    it "03. works with basic structure (d)" $
        qtest "03d-basic-structuring" $ Test03d.test

    it "04. formatting: headings (a)" $
        qtest "04a-formatting-headings" $ Test04a.test

    it "04. formatting: blocks (b)" $
        qtest "04b-formatting-blocks" $ Test04b.test

    it "04. formatting: lists (c)" $
        qtest "04c-formatting-lists" $ Test04c.test

    it "04. formatting: dates (g)" $
        qtest "04g-formatting-dates" $ Test04g.test

    it "04. formatting: footnotes (e)" $
        qtest "04e-formatting-footnotes" $ Test04e.test


qtest
    :: forall (m ∷ Type -> Type)
     . Bind m => MonadEffect m => MonadThrow _ m
    => String -> OrgFile -> m Unit
qtest fileSlug orgFile = do
    orgTestText <- liftEffect $ readTextFile UTF8 ("./test/examples/org-test/" <> fileSlug <> ".org")
    (D.render { break : D.All, indent : D.Spaces 1 } $ R.layout orgFile)
            `shouldEqual` orgTestText