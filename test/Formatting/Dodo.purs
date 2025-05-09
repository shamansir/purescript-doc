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


testA :: Dodo.Doc F.Directive
testA =
  D.words
    [ D.text "pre"
    , F.bold $ D.words
      [ D.text "bold"
      , F.underline $ D.text "bold&under"
      , D.text "bold2"
      ]
    , D.text "post"
    ]


testB :: Dodo.Doc F.Directive
testB =
  F.fgc red $ F.thru $ D.words
    [ D.text "This is"
    , F.fgc blue $ F.bold $ D.text "blue and bold"
    , F._null $ F.underline $ D.text "underlined text."
    , D.text "The end."
    ]


yellow = Color.rgb 0 255 255 :: Color
red = Color.rgb 255 0 0 :: Color
blue = Color.rgb 0 0 255 :: Color


testC :: Dodo.Doc F.Directive
testC =
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


{-
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
-}

qTest :: Dodo.Doc F.Directive -> String
qTest = Dodo.print FDodo.printer Dodo.twoSpaces


spec :: Spec Unit
spec = do

  describe "Formatting works properly for Dodo" $ do

    -- itOnly "0: simple text" $ do
    --   qTest (D.text "pre") `shouldEqual` "pre"

    -- itOnly "1: only formatted text" $ do
    --   qTest (F.bold $ D.text "bold") `shouldEqual` "**bold**"
    --   qTest (F.underline $ D.text "under") `shouldEqual` "__under__"

    -- itOnly "2: formatted text after usual text" $ do
    --   qTest (D.text "smth" <> (F.bold $ D.text "bold")) `shouldEqual` "smth**bold**"
    --   qTest (D.text "smth" <> (F.underline $ D.text "under")) `shouldEqual` "smth__under__"

    -- itOnly "3: formatted text before usual text" $ do
    --   qTest ((F.bold $ D.text "bold") <> D.text "smth") `shouldEqual` "**bold**smth"
    --   qTest ((F.underline $ D.text "under") <> D.text "smth") `shouldEqual` "__under__smth"

    itOnly "4: formatted inside formatted" $ do
      qTest (F.bold $ F.underline $ D.text "bold&under") `shouldEqual` "**__bold&under__**"
      qTest (F.underline $ F.bold $ D.text "bold&under") `shouldEqual` "__**bold&under**__"

    -- itOnly "5: formatted inside formatted, extended" $ do
    --   qTest (F.bold      (D.text "bold-start"  <> (F.underline $ D.text "bold&under") <> D.text "bold-end") ) `shouldEqual` "**bold-start__bold&under__bold-end**"
    --   qTest (F.underline (D.text "under-start" <> (F.bold      $ D.text "bold&under") <> D.text "under-end")) `shouldEqual` "__under-start**bold&under**under-end__"

    -- itOnly "4: complex test A" $ do
    --   qTest testA `shouldEqual` "pre **bold __bold&under__ bold2** post"

    -- itOnly "1: should equal" $ do
    --   Dodo.print FDodo.printer Dodo.twoSpaces test1 `shouldEqual` "AAA"

    -- it "2: should equal" $ do
    --   Dodo.print FDodo.printer Dodo.twoSpaces test2 `shouldEqual` "AAA"