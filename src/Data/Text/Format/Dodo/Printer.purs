module Data.Text.Format.Dodo.Printer where

import Prelude

import Debug

import Data.Text.Format.Dodo.Format (Directive) as F

import Dodo (Printer(..)) as Dodo


printer :: Dodo.Printer String F.Directive String
printer = Dodo.Printer
    { emptyBuffer : ""
    , enterAnnotation : \tag tags buff -> buff
    , leaveAnnotation : \tag tags buff -> buff
    , writeBreak : \buff -> buff <> "\n"
    , writeIndent : \width str buff -> buff <> str
    , writeText : \width str buff -> buff <> str
    , flushBuffer : \buff -> buff
    }