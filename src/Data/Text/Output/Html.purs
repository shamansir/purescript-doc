module Data.Text.Output.Html where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Text.Output (OutputKind, class Renderer, Support)


import Color as Color

import Type.Proxy (Proxy(..))

import Data.Array (intersperse, mapWithIndex) as Array
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Enum (toEnum)
import Data.Either (Either(..)) as E
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String
import Data.Text.Format (Tag(..), Format(..), Align(..), Term(..), Url(..), Level(..), Anchor(..), Bullet(..), FootnoteId(..), ProgrammingLanguage(..), bulletPrefix)
import Data.Text.Output (layout) as O
import Data.Text.Output (OutputKind, class Renderer, Support)
import Data.Text.Output (Support(..), perform) as S
import Data.Text.Doc (Doc, (<+>))
import Data.Text.Doc as D
import Data.DateTime (DateTime(..)) as DT
import Data.DateTime as DateTime
import Data.Date (Date(..), canonicalDate) as Dt
import Data.Date as Date
import Data.Time (Time(..)) as Tm
import Data.Time.Component (Hour(..), Minute(..), Millisecond(..)) as Tm
import Data.Time as Time
import Data.Formatter.DateTime as FDT


foreign import data Html :: OutputKind


html = Proxy :: _ Html


layout :: Tag -> Doc
layout = O.layout html


instance Renderer Html where

    supported :: Proxy Html -> Tag -> Support
    supported _ = case _ of
        Empty -> S.Full
        Plain _ -> S.Full
        Newline -> S.Full
        Format format _ ->
            case format of
                Fg _ -> S.Full
                Bg _ -> S.Full
                Bold -> S.Full
                Underline -> S.Full
                Blink -> S.Full
                Inverse -> S.Full
                Invisible -> S.Full
                Define _ -> S.Text
                Link _ -> S.Partly
                Image _ -> S.Text
                LinkTo _ -> S.Partly
                Comment -> S.Text
                _ -> S.Text -- TODO
        Split _ _ -> S.Partly
        Align _ _ -> S.Full
        Pair _ _ -> S.Full
        Para _ -> S.Full
        Nest _ _ -> S.Full
        Join _ _ -> S.Full
        Wrap _ _ _ -> S.Full
        List _ _ _ -> S.Text
        Table _ -> S.None
        Hr -> S.Text

    layout :: Proxy Html -> Tag -> Doc
    layout = const $ case _ of
        Empty -> D.nil
        Plain str -> D.text str
        Newline -> D.bracket "<" (D.text "br") "/>"
        Format format tag ->
            case format of
                Fg (E.Left colorStr) -> wrapS "span" ("color:" <> colorStr) tag
                Fg (E.Right color) -> wrapS "span" ("color:" <> Color.toHexString color) tag
                Bg (E.Left colorStr) -> wrapS "span" ("background-color:" <> colorStr) tag
                Bg (E.Right color) -> wrapS "span" ("background-color:" <> Color.toHexString color) tag
                Header (Level n) mbAnchor -> wrap' ("h" <> show (max 6 n)) $ case mbAnchor of
                    Just (Anchor anchor) -> wrapAttr "a" "href" ("#" <> anchor) tag
                    Nothing -> layout tag
                Bold -> wrap "bold" tag
                Underline -> wrap "ins" tag
                Strikethrough -> wrap "del" tag
                Monospaced -> wrap "code" tag
                Verbatim -> wrap "code" tag
                FixedWidth -> wrap "code" tag
                Code (ProgrammingLanguage lang) -> wrapAttr "code" "lang" lang tag
                Quote -> wrap "qoute" tag
                Sub -> wrap "sub" tag
                Sup -> wrap "sup" tag
                Blink -> wrap "blink" tag
                Inverse -> wrap "inverse" tag
                Invisible -> wrapS "span" "display:none" tag
                Define (Term dt) -> let dd = tag in wrap "dd" dd <> wrap "dt" dt
                LinkTo (FootnoteId (E.Left ftn)) -> wrapAttr "a" "href" ("#" <> show ftn) tag
                LinkTo (FootnoteId (E.Right ftn)) -> wrapAttr "a" "href" ("#" <> ftn) tag
                Link (Url url) -> wrapAttr "a" "href" url tag
                Image (Url url) -> wrapAttr "img" "src" url tag
                Comment -> D.bracket "<!--" (layout tag) "-->"
                Footnote (FootnoteId (E.Left ftn)) -> wrapAttr "a" "name" ("#" <> show ftn) tag
                Footnote (FootnoteId (E.Right ftn)) -> wrapAttr "a" "name" ("#" <> ftn) tag
                -- _ -> layout tag -- other format's are not supported
        Split tagA tagB -> D.nil -- layout tagA <> D.text "{|}" <> layout tagB
        Align Left tag -> wrapS "div" "text-align:left" tag
        Align Right tag -> wrapS "div" "text-align:right" tag
        Align Center tag -> wrapS "div" "text-align:center" tag
        Pair tagA tagB -> layout tagA <> layout tagB
        Para tags -> D.stack $ layout <$> tags
        Nest i tags -> D.nest' (unwrap i) $ ident i <$> tags
        Join tag tags -> D.folddoc (<>) $ layout <$> Array.intersperse tag tags
        Wrap start end tag -> D.bracket start (layout tag) end -- TODO: encode text
        List bullet Empty items ->
            wrapS' "ul" ("list-style-type:" <> case bullet of
                Asterisk -> "bullet"
                Alpha -> "lower-alpha"
                AlphaInv -> "none" -- FIXME
                Disc -> "disc"
                Circle -> "circle"
                Dash -> "dash"
                Num -> "decimal"
                None -> "none"
            ) $ D.stack $ wrap "li" <$> items
        List bullet start items ->
            wrapAttr "label" "for" "" start
            <> layout (List bullet Empty items)
        Table items -> D.nil -- TODO
        Hr -> wrapE "hr"
        where
            -- b bullet (index /\ doc) = bulletPrefix index bullet /\ doc
            ident n = wrapS "div" ("padding-left:" <> show n)
            wrap htmlTag = wrap' htmlTag <<< layout
            wrap' htmlTag content = D.bracket "<" (D.text htmlTag) ">" <> content <> D.bracket "</" (D.text htmlTag) ">"
            wrapAttr htmlTag attrName atrrVal = wrapAttr' htmlTag attrName atrrVal <<< layout
            wrapAttr' htmlTag attrName atrrVal content = D.bracket "<" (D.text htmlTag <> D.space <> D.text (attrName <> "=") <> D.wrap "\"" (D.text atrrVal)) ">" <> content <> D.bracket "</" (D.text htmlTag) ">"
            wrapS htmlTag = wrapAttr htmlTag "style"
            wrapS' htmlTag = wrapAttr' htmlTag "style"
            wrapE htmlTag = D.bracket "<" (D.text htmlTag) "/>"


singleLine :: Tag -> String
singleLine = layout >>> D.render { break : D.Space, indent : D.Empty }
-- render = S.perform blessed { break : D.Space, indent : D.Empty }


multiLine :: Tag -> String
multiLine = layout >>> D.render { break : D.All, indent : D.Spaces 2 }