module Test.Org.Export.Org where

import Prelude

import Effect.Class (liftEffect, class MonadEffect)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Maybe (Maybe(..))
-- import Data.Text.Doc as D
import Data.Text.Doc as D
import Data.Text.Format.Org.Types (OrgFile, Check(..))
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

    it "works with the syntax sample" $
        qtest "01-empty" $ Org.empty

    it "works with the meta sample" $
        qtest "02-meta"
            $ Org.metan 1 "title" "The glories of Org"
            $ Org.metan 2 "author" "A. Org Author"
            $ Org.empty


qtest
    :: forall (m ∷ Type -> Type)
     . Bind m => MonadEffect m => MonadThrow _ m
    => String -> OrgFile -> m Unit
qtest fileSlug orgFile = do
    orgTestText <- liftEffect $ readTextFile UTF8 ("./test/examples/org-test-" <> fileSlug <> ".org")
    (D.render { break : D.All, indent : D.Spaces 4 } $ R.layout orgFile)
            `shouldEqual` orgTestText