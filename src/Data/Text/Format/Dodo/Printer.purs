module Data.Text.Format.Dodo.Printer where

import Prelude

import Debug as Debug

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Text.Format.Dodo.Format (Directive) as F
import Data.Text.Format.Dodo.Seam as F
import Data.Text.Output.Markdown as Markdown

import Dodo (Printer(..)) as Dodo


type Buffer =
    { result :: F.Seam
    , entered :: List F.Directive
    , deep :: Int
    , content :: F.Seam
    }


initBuffer :: Buffer
initBuffer =
    { result : F.nil
    , entered : List.Nil
    , deep : 0
    , content : F.nil
    }

-- directiveRule :: forall content. (content -> S.Seam) -> X.Directive -> (content -> S.Seam)


apply :: F.Directive -> F.Seam -> F.Seam
apply = Markdown.directiveRule identity


applyTree :: F.Directive -> Array F.Seam -> F.Seam
applyTree = Markdown.directiveRule F.join


printer :: Dodo.Printer Buffer F.Directive String
printer = addDebugTrace $ Dodo.Printer
    { emptyBuffer : initBuffer
    , enterAnnotation : \tag tags buff ->
        buff
            { entered = List.snoc buff.entered tag
            , deep = buff.deep + 1
            }
    , leaveAnnotation : \tag tags buff ->
        if (buff.deep > 1)
            then
                buff
                    { entered = List.dropEnd 1 buff.entered
                    , deep = buff.deep - 1
                    , content = buff.content <> apply tag buff.content
                    }
            else
                buff
                    { entered = List.Nil
                    , deep = 0
                    , content = F.nil
                    , result = buff.result <> apply tag buff.content
                    }
    , writeBreak : \buff ->
        buff { content = buff.content <> F.break }
    , writeIndent : \width str buff ->
        buff { content = buff.content <> F.text str }
    , writeText : \width str buff ->
        buff { content = buff.content <> F.text str }
    , flushBuffer : _.result >>> F.render
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
            in v
        , leaveAnnotation : \tag tags buff ->
            let
                _ = Debug.spy "directive-leave: tag" tag
                -- _ = Debug.spy "directive-leave: tags" tags
                _ = Debug.spy "directive-leave: buff" $ show buff
                v = printer.leaveAnnotation tag tags buff
                _ = Debug.spy "directive-leave: return" $ show v
            in v
        , writeBreak : \buff ->
            let
                _ = Debug.spy "writeBreak: buff" $ show buff
                v = printer.writeBreak buff
                _ = Debug.spy "writeBreak: return" $ show v
            in v
        , writeIndent : \width str buff ->
            let
                -- _ = Debug.spy "writeIndent: width" width
                _ = Debug.spy "writeIndent: str" str
                _ = Debug.spy "writeIndent: buff" $ show buff
                v = printer.writeIndent width str buff
                _ = Debug.spy "writeIndent: return" $ show v
            in v
        , writeText : \width str buff ->
            let
                -- _ = Debug.spy "writeText: width" width
                _ = Debug.spy "writeText: str" str
                _ = Debug.spy "writeText: buff" $ show buff
                v = printer.writeText width str buff
                _ = Debug.spy "writeText: return" $ show v
            in v
        , flushBuffer : \buff ->
            let
                _ = Debug.spy "flushBuffer: buff" $ show buff
                v = printer.flushBuffer buff
                _ = Debug.spy "flushBuffer: return" v
            in v
        }