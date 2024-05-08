module Test.Org.Export.Org where

import Prelude

import Effect.Class (liftEffect, class MonadEffect)
import Control.Monad.Error.Class (class MonadThrow)

import Data.Maybe (Maybe(..))
-- import Data.Text.Doc as D
import Data.Text.Doc as D
import Data.Text.Format.Org.Types (OrgFile, Check(..), Todo(..), Priority(..), ListType(..))
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

    it "01. works with the syntax sample" $
        qtest "01-empty" $ Org.empty

    it "02. works with the meta sample (a)" $
        qtest "02a-meta"
            $ Org.metan 1 "title" "The glories of Org"
            $ Org.metan 2 "author" "A. Org Author"
            $ Org.empty

    it "02. works with the meta sample (b)" $
        qtest "02b-meta-special"
            $ Org.todoSequence
                ( Org.Progress <$> [ "NEXT", "TODO", "WAITING", "SOMEDAY", "PROJ" ])
                ( Org.Finish   <$> [ "DONE", "CANCELLED" ])
            $ Org.empty

    it "03. works with basic headings and levels (a)" $
        qtest "03a-headings-with-no-content"
            $ Org.f
                $ Org.ds
                    [ Org.sec 1 [ Org.text "First Level Heading" ] $
                        Org.ssec 2 [ Org.text "Second Level Heading" ] $
                            Org.ssec 3 [ Org.text "Third Level Heading" ] $
                                Org.ssec 4 [ Org.text "Fourth Level Heading" ] $
                                    Org.emptyDoc
                                    -- Org.db [ Org.para [ Org.br ] ]
                    ]

    it "03. works with heading with some content (b)" $
        qtest "03b-headings-with-content"
            $ Org.f
                $ Org.ds
                    [ Org.sec 1 [ Org.text "First Level Heading" ] $
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


    it "03. works with basic structure (c)" $
        qtest "03c-basic-structuring"
            $ Org.f
                $ Org.dbs
                    [ Org.para1 $ Org.text "An introduction." ]
                    [ Org.sec1 1 (Org.text "A Heading") $
                        Org.dbs
                            [ Org.para1 $ Org.text "Some text." ]
                            [ Org.sece1 2 (Org.text "Sub-Topic 1")
                            , Org.sece1 2 (Org.text "Sub-Topic 2")
                            , Org.sece1 3 (Org.text "Additional entry")
                            ]
                    , Org.sec1 1 (Org.text "") $
                        Org.ds
                            [ Org.set DONE $ Org.sec1 2 (Org.text "") $
                                Org.ssec 3 [ Org.text "Some e-mail" ] $
                                    Org.ds
                                        [ Org.sece1 4 (Org.text "Title")
                                            # Org.set TODO
                                            # Org.priority (Alpha 'A')
                                            # Org.tag "a2%"
                                            # Org.tag "tag"
                                            # Org.comment
                                        ]
                            ]
                    ]


    it "04. formatting: headings (a)" $
        qtest "04a-formatting-headings"
            $ Org.f
                $ Org.ds 
                    [ Org.sec1 1 (Org.text "Welcome to Org-mode")
                        $ Org.ssec1 2 (Org.text "Sub-heading") 
                            $ Org.db1 $ Org.para [ Org.text "Each extra ~*~ increases the depth by one level.", Org.br ]
                    , Org.set TODO $ Org.sec1 1 (Org.text "Promulgate Org to the world")
                        $ Org.ds1 
                            $ Org.set TODO $ Org.sece1 2 $ Org.text "Create a quickstart guide"  
                    ]

    it "04. formatting: blocks (b)" $
        qtest "04b-formatting-blocks"
            $ Org.f 
                $ Org.db 
                    [ Org.para 
                        [ Org.text "To markup text in Org, simply surround it with one or more marker characters."
                        , Org.br, Org.b "Bold", Org.text ", ", Org.i "italic", Org.text " and ", Org.u "underline", Org.text " are fairly intuitive, and the ability to use", Org.br
                        , Org.s "strikethrough", Org.text " is a plus.  You can ", Org.marked (Org.both Org.bold $ Org.both Org.italic Org.under) "combine", Org.text " the basic markup in any", Org.br
                        , Org.text "order, however ", Org.ic "code", Org.text " and ", Org.v "verbatim", Org.text " need to be the ", Org.marked (Org.both Org.icode $ Org.both Org.under Org.bold) "inner-most", Org.text " markers", Org.br
                        , Org.text "if they are present since their contents are interpreted ", Org.v "_literally_", Org.text "."
                        ]
                    , Org.blank
                    , Org.para 
                        [ Org.to (Org.rem "https://orgmode.org") "a nice website", Org.br 
                        , Org.ref (Org.loc "~/Pictures/dank-meme.png"), Org.br 
                        , Org.to (Org.head "earlier heading") "an earlier heading in the document", Org.br 
                        ]
                    , Org.para1 $ Org.img_ (Org.irem "https://upload.wikimedia.org/wikipedia/commons/5/5d/Konigsberg_bridges.png")
                    , Org.blank 
                    , Org.example "monospace"
                    , Org.blank 
                    , Org.codeIn "emacs-lisp" "(message \"Hello world\")"
                    , Org.blank
                    , Org.quote "Everything should be made as simple as possible,\nbut not any simpler ---Albert Einstein"
                    ]

    it "04. formatting: lists (c)" $ 
        qtest "04c-formatting-lists"
            $ Org.f
                $ Org.dbs 
                    [ Org.para1 $ Org.text "To buy:" 
                    , Org.list Numbered 
                        [ Org.item1 $ Org.text "Milk" 
                        , (Org.item1 $ Org.text "Eggs") 
                            # Org.sub Hyphened
                                [ Org.item1 $ Org.text "Organic" 
                                ]
                        , (Org.item1 $ Org.text "Cheese") 
                            # Org.sub Plussed
                                [ Org.item1 $ Org.text "Parmesan"
                                , Org.item1 $ Org.text "Mozzarella"
                                ]
                        ]
                    , Org.blank
                    , Org.list Hyphened
                        [ Org.check Uncheck $ Org.item1 $ Org.text "not started"
                        , Org.check Halfcheck $ Org.item1 $ Org.text "in progress"
                        , Org.check Check $ Org.item1 $ Org.text "complete"
                        ]
                    , Org.blank
                    , Org.list Hyphened
                        [ Org.tagi "fruits" $ Org.check Uncheck $ Org.item1 $ Org.text "get apples" 
                        , Org.tagi "veggies" $ Org.check Check $ Org.item1 $ Org.text "get carrots" 
                        ]
                    , Org.blank   
                    , Org.list Alphed 
                        [ Org.item1 $ Org.text "First"
                        , Org.item1 $ Org.text "Second"
                        , Org.item1 $ Org.text "Third"
                        , Org.item1 $ Org.text "Fourth"
                        ]                        
                    , Org.blank
                    , Org.list Hyphened 
                        [ Org.item1 $ Org.text "item" ] 
                    , Org.list (NumberedFrom 3) 
                        [ Org.count 3 $ Org.item1 $ Org.text "set to three" ] 
                    , Org.list Plussed 
                        [ (Org.check Halfcheck $ Org.tagi "tag" $ Org.item1 $ Org.text "item contents")
                            # Org.sub Bulleted 
                                [ Org.item1 $ Org.text "item, note whitespace in front" ] 
                        ]
                    ]
                    [ Org.sece1 1 $ Org.text "not an item, but heading - heading takes precedence"
                    ]


qtest
    :: forall (m ∷ Type -> Type)
     . Bind m => MonadEffect m => MonadThrow _ m
    => String -> OrgFile -> m Unit
qtest fileSlug orgFile = do
    orgTestText <- liftEffect $ readTextFile UTF8 ("./test/examples/org-test/org-test-" <> fileSlug <> ".org")
    (D.render { break : D.All, indent : D.Spaces 1 } $ R.layout orgFile)
            `shouldEqual` orgTestText