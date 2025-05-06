module Test.Formatting.Dodo where

import Prelude

import Color as Color

import Effect (Effect)
import Effect.Class.Console as Console

import Data.Text.Format as F
import Data.Text.Format.Dodo as FDodo

import Dodo (Doc, print, twoSpaces) as Dodo
import Dodo (text, indent, lines, annotate, words) as D
import Dodo.Ansi as Ansi

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

test1 :: Dodo.Doc F.Tag
test1 =
  D.annotate (F.fgc (Color.rgb 255 0 0) $ F.s "Red")
    $ D.annotate (F.thru $ F.s "Thru") $ D.words
    [ D.text "This is"
    , D.annotate (F.fgc (Color.rgb 0 0 255) $ F.s "Blue")
        $ D.annotate (F.bold $ F.s "foo") $ D.text "bold"
    , D.annotate (F.underline $ F.s "Underline") $ D.text "text."
    , D.text "The end."
    ]


test2 :: Dodo.Doc F.Tag
test2 =
  D.lines
    [ D.text "Line with no style"
    , D.annotate
        (F.bgc (Color.rgb 255 255 0) $ F.s "YellowTest")
        $ D.annotate (F.bold $ F.s "BoldTest") $ D.lines
            [ D.annotate (F.thru $ F.s "Haha") $ D.text "strikethrough"
            , D.indent $ D.annotate (F.fgc (Color.rgb 255 0 0) $ F.s "Red")
              $ D.lines
                [ D.text "Red"
                , D.annotate F.hr $ D.text "Lines"
                , D.text "And bold"
                ]
            ]
    ]


test1Ansi :: Dodo.Doc Ansi.GraphicsParam
test1Ansi =
  Ansi.foreground Ansi.Red $ Ansi.strikethrough $ D.words
    [ D.text "This is"
    , Ansi.foreground Ansi.Blue $ Ansi.bold $ D.text "bold"
    , Ansi.reset $ Ansi.underline $ D.text "text."
    , D.text "The end."
    ]


test2Ansi :: Dodo.Doc Ansi.GraphicsParam
test2Ansi =
  D.lines
    [ D.text "Line with no style"
    , Ansi.background Ansi.Yellow $ Ansi.bold $ D.lines
        [ Ansi.strikethrough $ D.text "Strikethrough"
        , D.indent $ Ansi.foreground Ansi.Red $ D.lines
            [ D.text "Red"
            , Ansi.reset $ D.text "Lines"
            , D.text "And bold"
            ]
        , D.text "The end."
        ]
    ]


spec :: Spec Unit
spec = do

  describe "Formatting works properly for Dodo" $ do

    it "1: should equal" $ do
      Dodo.print FDodo.printer Dodo.twoSpaces test1 `shouldEqual` "AAA"

    it "2: should equal" $ do
      Dodo.print FDodo.printer Dodo.twoSpaces test2 `shouldEqual` "AAA"