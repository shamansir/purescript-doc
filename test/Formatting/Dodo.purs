module Test.Formatting.Dodo where

import Prelude

import Color (Color)
import Color (rgb) as Color

import Effect (Effect)
import Effect.Class.Console as Console

import Data.Text.Format.Dodo.Format as F
import Data.Text.Format.Dodo.Printer as FDodo

import Dodo (Doc, print, twoSpaces) as Dodo
import Dodo (text, indent, lines, words) as D
import Dodo.Ansi as Ansi

import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)


simplest :: Dodo.Doc F.Directive
simplest =
  F.bold $ D.words
    [ D.text "bold1"
    , F.em $ D.text "bold+em"
    , D.text "bold2"
    ]


test1 :: Dodo.Doc F.Directive
test1 =
  F.fgc red $ F.thru $ D.words
    [ D.text "This is"
    , F.fgc blue $ F.bold $ D.text "blue and bold"
    , F._null $ F.underline $ D.text "underlined text."
    , D.text "The end."
    ]


yellow = Color.rgb 0 255 255 :: Color
red = Color.rgb 255 0 0 :: Color
blue = Color.rgb 0 0 255 :: Color


test2 :: Dodo.Doc F.Directive
test2 =
  D.lines
    [ D.text "Line with no style"
    , F.bgc yellow $ F.bold $ D.lines
            [ F.thru $ D.text "strikethrough"
            , D.indent $ F.fgc red
              $ D.lines
                [ D.text "Red"
                , F._null $ D.text "Lines"
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

    itOnly "0: should equal" $ do
      Dodo.print FDodo.printer Dodo.twoSpaces simplest `shouldEqual` "AAA"

    -- itOnly "1: should equal" $ do
    --   Dodo.print FDodo.printer Dodo.twoSpaces test1 `shouldEqual` "AAA"

    -- it "2: should equal" $ do
    --   Dodo.print FDodo.printer Dodo.twoSpaces test2 `shouldEqual` "AAA"