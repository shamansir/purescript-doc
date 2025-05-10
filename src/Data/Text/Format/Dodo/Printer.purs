module Data.Text.Format.Dodo.Printer where

import Prelude

import Debug as Debug

import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Format.Dodo.Format (Directive) as F
-- import Data.Text.Format.Dodo.Seam as F
import Data.Text.Format.Dodo.SeamAlt as SA
import Data.Text.Output.Markdown as Markdown

import Dodo (Printer(..)) as Dodo


type Buffer = String


initBuffer :: Buffer
initBuffer = ""

-- directiveRule :: forall content. (content -> S.Seam) -> X.Directive -> (content -> S.Seam)


withLast :: forall a. List a -> (a -> a) -> List a
withLast list f = fromMaybe list $ List.modifyAt (List.length list - 1) f list


applyStart :: F.Directive -> String
applyStart = Markdown.directiveRuleAlt >>> case _ of
    SA.Nil -> ""
    SA.Single markup -> markup
    -- SA.IndentBefore
    SA.SurroundInline { spaced, left } ->
        if spaced then left <> " " else left
    SA.SurroundBlock { above } ->
        above <> "\n"
    SA.Mark { marker, spaced } ->
        if spaced then marker <> " " else marker
    SA.HtmlTag { name, attrs } ->
        case attrs of
            [] -> "<" <> name <> ">"
            theAttrs ->
                "<" <> name <> " "
                    <> String.joinWith " " ((\(attrName /\ attrValue) -> attrName <> "=\"" <> attrValue <> "\"") <$> theAttrs) <>
                ">"


applyEnd :: F.Directive -> String
applyEnd = Markdown.directiveRuleAlt >>> case _ of
    SA.Nil -> ""
    SA.Single _ -> ""
    -- SA.IndentBefore
    SA.SurroundInline { spaced, right } ->
        if spaced then " " <> right else right
    SA.SurroundBlock { below } ->
        "\n" <> below
    SA.Mark { marker, spaced } ->
        ""
    SA.HtmlTag { name, attrs } ->
        "</" <> name <> ">"


printer :: Dodo.Printer Buffer F.Directive String
printer = addDebugTrace $ Dodo.Printer
    { emptyBuffer : initBuffer
    , enterAnnotation : \tag tags buff ->
        buff <> applyStart tag
    , leaveAnnotation : \tag tags buff ->
        buff <> applyEnd tag
    , writeBreak : \buff ->
        buff <> "\n"
        -- buff { content = buff.content <> F.break }
    , writeIndent : \width str buff ->
        buff <> str
    , writeText : \width str buff ->
        buff <> str
        -- buff { content = buff.content <> F.text str }
    , flushBuffer : identity
    }


addDebugTrace :: forall buf ann res. Show buf => Dodo.Printer buf ann res -> Dodo.Printer buf ann res
addDebugTrace (Dodo.Printer printer) =
    Dodo.Printer
        { emptyBuffer : Debug.spy "init" printer.emptyBuffer
        , enterAnnotation : \tag tags buff ->
            let
                _ = Debug.spy "directive-enter: tag" tag
                -- _ = Debug.spy "directive-enter: tags" tags
                _ = Debug.spy "directive-enter: buff" $ show buff
                v = printer.enterAnnotation tag tags buff
                _ = Debug.spy "directive-enter: return" $ show v
                _ = Debug.spy "--------" "--------"
            in v
        , leaveAnnotation : \tag tags buff ->
            let
                _ = Debug.spy "directive-leave: tag" tag
                -- _ = Debug.spy "directive-leave: tags" tags
                _ = Debug.spy "directive-leave: buff" $ show buff
                v = printer.leaveAnnotation tag tags buff
                _ = Debug.spy "directive-leave: return" $ show v
                _ = Debug.spy "--------" "--------"
            in v
        , writeBreak : \buff ->
            let
                _ = Debug.spy "writeBreak: buff" $ show buff
                v = printer.writeBreak buff
                _ = Debug.spy "writeBreak: return" $ show v
                _ = Debug.spy "--------" "--------"
            in v
        , writeIndent : \width str buff ->
            let
                -- _ = Debug.spy "writeIndent: width" width
                _ = Debug.spy "writeIndent: str" str
                _ = Debug.spy "writeIndent: buff" $ show buff
                v = printer.writeIndent width str buff
                _ = Debug.spy "writeIndent: return" $ show v
                _ = Debug.spy "--------" "--------"
            in v
        , writeText : \width str buff ->
            let
                -- _ = Debug.spy "writeText: width" width
                _ = Debug.spy "writeText: str" str
                _ = Debug.spy "writeText: buff" $ show buff
                v = printer.writeText width str buff
                _ = Debug.spy "writeText: return" $ show v
                _ = Debug.spy "--------" "--------"
            in v
        , flushBuffer : \buff ->
            let
                _ = Debug.spy "flushBuffer: buff" $ show buff
                v = printer.flushBuffer buff
                _ = Debug.spy "flushBuffer: return" v
                _ = Debug.spy "--------" "--------"
            in v
        }