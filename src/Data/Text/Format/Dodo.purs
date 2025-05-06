module Data.Text.Format.Dodo where

import Prelude

import Data.Text.Format

import Dodo (Printer(..)) as Dodo


printer :: Dodo.Printer String Tag String
printer = Dodo.Printer
    { emptyBuffer : ""
    , enterAnnotation : \tag tags buff -> buff
    , leaveAnnotation : \tag tags buff -> buff
    , writeBreak : \buff -> buff <> "\n"
    , writeIndent : \width str buff -> buff <> str
    , writeText : \width str buff -> buff <> str
    , flushBuffer : \buff -> buff
    }