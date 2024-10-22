module Data.Text.Output.Markdown where

import Prelude

import Color as Color

import Type.Proxy (Proxy(..))

import Data.Array (intersperse, mapWithIndex) as Array
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..)) as E
import Data.Newtype (unwrap)

import Data.Text.Format
    ( Tag(..), Format(..), Align(..), Term(..), Definition(..), TermAndDefinition(..)
    , Url(..), Level(..), Anchor(..), FootnoteId(..), ProgrammingLanguage(..)
    , Indent(..), ImageParams(..), ImageSide(..), QuoteOf(..)
    , bulletPrefix)
import Data.Text.Output (layout) as O
import Data.Text.Output (OutputKind, class Renderer, Support)
import Data.Text.Output (Support(..)) as S
import Data.Text.Doc (Doc)
import Data.Text.Doc as D


foreign import data Markdown :: OutputKind


markdown = Proxy :: _ Markdown


layout :: Tag -> Doc
layout = O.layout markdown


instance Renderer Markdown where

    supported :: Proxy Markdown -> Tag -> Support
    supported _ = case _ of
        _ -> S.Full -- FIXME

    layout :: Proxy Markdown -> Tag -> Doc
    layout = const $ case _ of
        Empty -> D.nil
        Plain str -> D.text str
        Newline -> D.break
        Format format tag ->
            case format of
                Fg (E.Left colorStr) -> wrapS "span" ("color:" <> colorStr) tag
                Fg (E.Right color) -> wrapS "span" ("color:" <> Color.toHexString color) tag
                Bg (E.Left colorStr) -> wrapS "span" ("background-color:" <> colorStr) tag
                Bg (E.Right color) -> wrapS "span" ("background-color:" <> Color.toHexString color) tag
                Header (Level n) mbAnchor ->
                    D.repeat n (D.text "#") <> D.space <> layout tag <> case mbAnchor of
                        Just (Anchor anchor) -> D.space <> D.bracket "{#" (D.text anchor) "}"
                        Nothing -> D.nil
                Bold -> D.wrap "**" $ layout tag
                Emphasis -> D.wrap "*" $ layout tag
                Highlight -> D.wrap "==" $ layout tag
                Underline -> D.wrap "__" $ layout tag
                Strikethrough -> D.wrap "~~" $ layout tag
                Monospaced -> D.wrapbr "```" $ layout tag
                Verbatim -> D.wrapbr "```" $ layout tag
                FixedWidth -> D.wrapbr "```" $ layout tag
                Code (ProgrammingLanguage lang) -> D.bracketbr ("```" <> lang) (layout tag) "```"
                Quote mbAuthor ->
                    (D.mark ">" $ layout tag) <>
                        case mbAuthor of
                            Just (QuoteOf author) -> D.space <> (D.mark "--" $ D.text author) -- D.break <> (D.mark ">" $ D.mark "--" $ D.text author)
                            Nothing -> D.nil
                     -- TODO: split by \n and append `>` for every line
                Sub -> D.wrap "~" $ layout tag
                Sup -> D.wrap "^" $ layout tag
                Blink -> wrap "blink" tag
                Inverse -> wrap "inverse" tag
                Invisible -> wrapS "span" "display:none" tag
                Define (Term dt) -> layout dt <> D.break <> D.mark ":" (layout tag)
                LinkTo (FootnoteId (E.Left ftn)) -> layout tag <> D.space <> D.bracket "[^" (D.text $ show ftn) "]"
                LinkTo (FootnoteId (E.Right ftn)) -> layout tag <> D.space <> D.bracket "[^" (D.text ftn) "]"
                Link (Url url) -> D.bracket "[" (layout tag) "]" <> D.bracket "(" (D.text url) ")"
                InlineImage (ImageParams params) (Url url) ->
                    case params.width /\ params.height of
                        Auto /\ Auto -> D.bracket "![" (layout tag) "]" <> D.bracket "(" (D.text url) ")" -- FIXME: use caption for the title
                        Px wpx /\ Px hpx -> wrapAttrsE "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ show hpx, "alt" /\ unwrap params.caption, inlineBlockStyle ] tag
                        Px wpx /\ Auto ->   wrapAttrsE "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ "auto", "alt" /\ unwrap params.caption, inlineBlockStyle ] tag
                        Auto /\ Px hpx ->   wrapAttrsE "img" [ "src" /\ url, "width" /\ "auto", "height" /\ show hpx, "alt" /\ unwrap params.caption, inlineBlockStyle ] tag
                Comment -> D.bracket "<!--" (layout tag) "-->"
                Footnote (FootnoteId (E.Left ftn)) -> D.bracket "[^" (D.text $ show ftn) "]" <> D.text ":" <> D.space <> layout tag
                Footnote (FootnoteId (E.Right ftn)) -> D.bracket "[^" (D.text ftn) "]" <> D.text ":" <> D.space <> layout tag
        Split tagA tagB ->layout tagA <> D.text "|" <> layout tagB
        Align Left tag -> wrapS "div" "text-align:left" tag
        Align Right tag -> wrapS "div" "text-align:right" tag
        Align Center tag -> wrapS "div" "text-align:center" tag
        Pair tagA tagB -> layout tagA <> layout tagB
        Para tags -> D.stack $ layout <$> tags
        Nest (Indent i) tags -> D.nest' i $ layout <$> tags
        Join tag tags -> D.folddoc (<>) $ layout <$> Array.intersperse tag tags
        Wrap start end tag -> D.bracket' (layout start) (layout tag) (layout end) -- TODO: encode text
        List bullet Empty items ->
            D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout <$> items)
        List bullet start items ->
            D.nest' 0 $ -- FIXME: support levels from `Nest`
                [ layout start
                , D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout <$> items)
                ]
        DefList definitions ->
            D.joinWith (D.break <> D.break) $ def <$> definitions
        Table headers rows ->
            wrap' "table"
                $ (wrap' "thead" $ D.stack $ wrap "th" <$> headers)
                <> D.break <> D.stack (wrap' "tr" <$> D.stack <$> map (wrap "td") <$> rows)
        Image (ImageParams params) (Url url) ->
            case params.width /\ params.height of
                Auto /\ Auto ->     D.bracket "![" (D.text $ unwrap params.caption) "]" <> D.bracket "(" (D.text url) ")"
                Px wpx /\ Px hpx -> wrapAttrsE' "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ show hpx, "alt" /\ unwrap params.caption ]
                Px wpx /\ Auto ->   wrapAttrsE' "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ "auto", "alt" /\ unwrap params.caption ]
                Auto /\ Px hpx ->   wrapAttrsE' "img" [ "src" /\ url, "width" /\ "auto", "height" /\ show hpx, "alt" /\ unwrap params.caption ]
        Hr -> D.text "---------"
        where
            b bullet (index /\ doc) = bulletPrefix index bullet /\ doc
            wrap htmlTag = wrap' htmlTag <<< layout
            wrap' htmlTag content = D.bracket "<" (D.text htmlTag) ">" <> content <> D.bracket "</" (D.text htmlTag) ">"
            wrapAttr htmlTag attrName atrrVal = wrapAttr' htmlTag attrName atrrVal <<< layout
            wrapAttr' htmlTag attrName attrVal content = D.bracket "<" (D.text htmlTag <> D.space <> makeAttr (attrName /\ attrVal)) ">" <> content <> D.bracket "</" (D.text htmlTag) ">" -- TODO: wrapAttrs Array.singleton
            wrapAttrs htmlTag attrs = wrapAttrs' htmlTag attrs <<< layout
            wrapAttrs' htmlTag attrs content = D.bracket "<" (D.text htmlTag <> D.space <> D.joinWith D.space (makeAttr <$> attrs)) ">" <> content <> D.bracket "</" (D.text htmlTag) ">"
            wrapAttrsE htmlTag attrs content = wrapAttrsE' htmlTag attrs <> layout content
            wrapAttrsE' htmlTag attrs = D.bracket "<" (D.text htmlTag <> D.space <> D.joinWith D.space (makeAttr <$> attrs)) "/>"
            makeAttr (attrName /\ attrVal) = D.text (attrName <> "=") <> D.wrap "\"" (D.text attrVal)
            wrapS htmlTag = wrapAttr htmlTag "style"
            def (TAndD (Term term /\ Definition definition)) = layout term <> D.break <> (D.mark ": " $ layout definition)
            inlineBlockStyle = "style" /\ "display:inline-block"