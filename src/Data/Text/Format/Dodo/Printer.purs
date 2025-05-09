module Data.Text.Format.Dodo.Printer where

import Prelude

import Debug as Debug

import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Text.Format.Dodo.Format (Directive) as F
import Data.Text.Format.Dodo.Seam as F
import Data.Text.Output.Markdown as Markdown

import Dodo (Printer(..)) as Dodo


type Buffer =
    { result :: F.Seam
    , deep :: Int
    , content :: Map Int F.Seam
    }


initBuffer :: Buffer
initBuffer =
    { result : F.nil
    , deep : 0
    , content : Map.empty
    }

-- directiveRule :: forall content. (content -> S.Seam) -> X.Directive -> (content -> S.Seam)


apply :: F.Directive -> F.Seam -> F.Seam
apply = Markdown.directiveRule identity


applyTree :: F.Directive -> Array F.Seam -> F.Seam
applyTree = Markdown.directiveRule F.join


withLast :: forall a. List a -> (a -> a) -> List a
withLast list f = fromMaybe list $ List.modifyAt (List.length list - 1) f list


printer :: Dodo.Printer Buffer F.Directive String
printer = addDebugTrace $ Dodo.Printer
    { emptyBuffer : initBuffer
    , enterAnnotation : \tag tags buff ->
        buff
            { deep = buff.deep + 1
            }
    , leaveAnnotation : \tag tags buff ->
        buff
            { deep = max (buff.deep - 1) 0
            , content = Map.delete buff.deep buff.content
            , result = case Map.lookup buff.deep buff.content of
                Just content -> buff.result <> apply tag content
                Nothing -> buff.result
            }
    , writeBreak : \buff ->
        buff { content = Map.update (\content -> Just $ content <> F.break) buff.deep buff.content }
        -- buff { content = buff.content <> F.break }
    , writeIndent : \width str buff ->
        buff { content = Map.update (\content -> Just $ content <> F.text str) buff.deep buff.content }
        -- buff { content = buff.content <> F.text str }
    , writeText : \width str buff ->
        buff { content = Map.update (\content -> Just $ content <> F.text str) buff.deep buff.content }
        -- buff { content = buff.content <> F.text str }
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