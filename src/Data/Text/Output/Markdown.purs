module Data.Text.Output.Markdown where

import Prelude

import Color as Color

import Type.Proxy (Proxy(..))

import Data.Array (intersperse, mapWithIndex) as Array
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..)) as E

import Data.Text.Format (Tag(..), Format(..), Align(..), Term(..), Url(..), Level(..), Anchor(..), FootnoteId(..), ProgrammingLanguage(..), Indent(..), bulletPrefix)
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
                Quote -> D.mark ">" $ layout tag -- TODO: split by \n and append `>` for every line
                Sub -> D.wrap "~" $ layout tag
                Sup -> D.wrap "^" $ layout tag
                Blink -> wrap "blink" tag
                Inverse -> wrap "inverse" tag
                Invisible -> wrapS "span" "display:none" tag
                Define (Term dt) -> layout dt <> D.break <> D.mark ":" (layout tag)
                LinkTo (FootnoteId (E.Left ftn)) -> layout tag <> D.space <> D.bracket "[^" (D.text $ show ftn) "]"
                LinkTo (FootnoteId (E.Right ftn)) -> layout tag <> D.space <> D.bracket "[^" (D.text ftn) "]"
                Link (Url url) -> D.bracket "[" (layout tag) "]" <> D.bracket "(" (D.text url) ")"
                Image (Url url) -> D.bracket "![" (layout tag) "]" <> D.bracket "(" (D.text url) ")"
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
        Wrap start end tag -> D.bracket start (layout tag) end -- TODO: encode text
        List bullet Empty items ->
            D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout <$> items)
        List bullet start items ->
            D.nest' 0 $ -- FIXME: support levels from `Nest`
                [ layout start
                , D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout <$> items)
                ]
        Table headers rows ->
            wrap' "table"
                $ (wrap' "thead" $ D.stack $ wrap "th" <$> headers)
                <> D.break <> D.stack (wrap' "tr" <$> D.stack <$> map (wrap "td") <$> rows)
        Hr -> D.text "---------"
        where
            b bullet (index /\ doc) = bulletPrefix index bullet /\ doc
            wrap htmlTag = wrap' htmlTag <<< layout
            wrap' htmlTag content = D.bracket "<" (D.text htmlTag) ">" <> content <> D.bracket "</" (D.text htmlTag) ">"
            wrapAttr htmlTag attrName atrrVal = wrapAttr' htmlTag attrName atrrVal <<< layout
            wrapAttr' htmlTag attrName atrrVal content = D.bracket "<" (D.text htmlTag <> D.space <> D.text (attrName <> "=") <> D.wrap "\"" (D.text atrrVal)) ">" <> content <> D.bracket "</" (D.text htmlTag) ">"
            wrapS htmlTag = wrapAttr htmlTag "style"