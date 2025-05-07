module Data.Text.Format.Dodo.Printer where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..))
import Data.Text.Format.Dodo.Format (Directive) as F

import Dodo (Printer(..)) as Dodo


type Buffer =
    { deep :: Int
    , last :: Maybe F.Directive
    , result :: String
    }


initBuffer :: Buffer
initBuffer =
    { deep : 0
    , last : Nothing
    , result : ""
    }


printer :: Dodo.Printer Buffer F.Directive String
printer = addDebugTrace $ Dodo.Printer
    { emptyBuffer : initBuffer
    , enterAnnotation : \tag tags buff ->
        buff { last = Just tag, deep = buff.deep + 1 }
    , leaveAnnotation : \tag tags buff ->
        buff { last = Nothing, deep = buff.deep - 1 }
    , writeBreak : \buff ->
        buff { result = buff.result <> "\n" }
    , writeIndent : \width str buff ->
        buff { result = buff.result <> str }
    , writeText : \width str buff ->
        buff { result = buff.result <> str }
    , flushBuffer : _.result
    }


addDebugTrace :: forall buf ann res. Dodo.Printer buf ann res -> Dodo.Printer buf ann res
addDebugTrace (Dodo.Printer printer) =
    Dodo.Printer
        { emptyBuffer : Debug.spy "init" printer.emptyBuffer
        , enterAnnotation : \tag tags buff ->
            let
                _ = Debug.spy "annotate: tag" tag
                _ = Debug.spy "annotate: tags" tags
                _ = Debug.spy "annotate: buff" buff
            in
            printer.enterAnnotation tag tags buff
        , leaveAnnotation : \tag tags buff ->
            let
                _ = Debug.spy "leave: tag" tag
                _ = Debug.spy "leave: tags" tags
                _ = Debug.spy "leave: buff" buff
            in
            printer.leaveAnnotation tag tags buff
        , writeBreak : \buff ->
            let
                _ = Debug.spy "writeBreak: buff" buff
            in
            printer.writeBreak buff
        , writeIndent : \width str buff ->
            let
                _ = Debug.spy "writeIndent: width" width
                _ = Debug.spy "writeIndent: str" str
                _ = Debug.spy "writeIndent: buff" buff
            in
            printer.writeIndent width str buff
        , writeText : \width str buff ->
            let
                _ = Debug.spy "writeText: width" width
                _ = Debug.spy "writeText: str" str
                _ = Debug.spy "writeText: buff" buff
            in
            printer.writeText width str buff
        , flushBuffer : \buff ->
            let
                _ = Debug.spy "flushBuffer" buff
            in
            Debug.spy "flushBuffer" $ printer.flushBuffer buff
        }