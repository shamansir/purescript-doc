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

    it "works with the meta sample (a)" $
        qtest "02a-meta"
            $ Org.metan 1 "title" "The glories of Org"
            $ Org.metan 2 "author" "A. Org Author"
            $ Org.empty

    it "works with the meta sample (b)" $
        qtest "02b-meta-special"
            $ Org.todoSequence
                ( Org.Progress <$> [ "NEXT", "TODO", "WAITING", "SOMEDAY", "PROJ" ])
                ( Org.Finish   <$> [ "DONE", "CANCELLED" ])
            $ Org.empty

    it "works with basic strucure (a)" $
        qtest "03a-basic-structuring"
            $ Org.f
                $ Org.ds
                    [ Org.sec 1 [ Org.text "First Level Heading" ] $
                        Org.ssec 2 [ Org.text "Second Level Heading" ] $
                            Org.ssec 3 [ Org.text "Third Level Heading" ] $
                                Org.ssec 4 [ Org.text "Fourth Level Heading" ] $
                                    Org.db [ Org.para [ Org.br ] ]
                    , Org.sec 1 [ Org.text "First Level Heading" ] $
                        Org.dbs
                            [ Org.para
                                [ Org.text "Some text here", Org.br
                                , Org.text "And another line", Org.br
                                ]
                            , Org.para
                                [ Org.text "And another paragraph"
                                ]
                            ]
                            [ Org.sec 2 [ Org.text "Second Level Heading" ] $
                                Org.dbs
                                    [ Org.para [ Org.text "Here some text as well" ] ]
                                    [ Org.sec 3 [ Org.text "Third Level Heading" ] $
                                        Org.dbs
                                            [ Org.para [ Org.text "The paragraph inside the third level" ] ]
                                            [ Org.sec 4 [ Org.text "Fourth Level Heading" ] $
                                                Org.db
                                                    [ Org.para [ Org.text "Let's see if we have text here" ] ]
                                            ]
                                    ]
                            ]
                    ]


    it "works with basic strucure (b)" $
        qtest "03b-basic-structuring"
            $ Org.f
                $ Org.ds
                    [ Org.sec 1 [] $
                        Org.ssec 2 [ ] $ -- FIXME: use TODO tag here
                            Org.ssec 3 [ Org.text "Some e-mail" ] $
                                Org.ssec 4 [ Org.text "Title" ] $
                                    Org.emptyDoc
                    ]


qtest
    :: forall (m ∷ Type -> Type)
     . Bind m => MonadEffect m => MonadThrow _ m
    => String -> OrgFile -> m Unit
qtest fileSlug orgFile = do
    orgTestText <- liftEffect $ readTextFile UTF8 ("./test/examples/org-test/org-test-" <> fileSlug <> ".org")
    (D.render { break : D.All, indent : D.Spaces 4 } $ R.layout orgFile)
            `shouldEqual` orgTestText