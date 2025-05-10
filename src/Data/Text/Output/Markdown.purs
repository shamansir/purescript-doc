module Data.Text.Output.Markdown where

import Prelude

import Color as Color

import Type.Proxy (Proxy(..))

import Data.Array (intersperse, mapWithIndex, replicate) as Array
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..)) as E
import Data.Newtype (unwrap)
import Data.String (joinWith) as String

import Data.Text.Format
    ( Tag(..), Format(..), Align(..), Term(..), Definition(..), TermAndDefinition(..)
    , Url(..), HLevel(..), Anchor(..), FootnoteId(..), ProgrammingLanguage(..)
    , Indent(..), ImageParams(..), ImageSide(..), QuoteOf(..)
    , bulletPrefix, hLevelToInt)
import Data.Text.Output (layout) as O
import Data.Text.Output (OutputKind, class Renderer, Support)
import Data.Text.Output (Support(..)) as Sup
import Data.Text.Doc (Doc)
import Data.Text.Doc as D


import Data.Text.Format.Dodo.Format
    ( Directive(..), Format(..), Align(..), Term(..), Definition(..), TermAndDefinition(..)
    , Url(..), HLevel(..), Anchor(..), FootnoteId(..), ProgrammingLanguage(..)
    , ImageParams(..), ImageSide(..), QuoteOf(..)
    , hLevelToInt ) as X
import Data.Text.Format.Dodo.Seam as S
import Data.Text.Format.Dodo.SeamAlt as SA


foreign import data Markdown :: OutputKind


markdown = Proxy :: _ Markdown


layout :: Tag -> Doc
layout = O.layout markdown


instance Renderer Markdown where

    supported :: Proxy Markdown -> Tag -> Support
    supported _ = case _ of
        _ -> Sup.Full -- FIXME

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
                Header hLevel mbAnchor ->
                    D.repeat (hLevelToInt hLevel) (D.text "#") <> D.space <> layout tag <> case mbAnchor of
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
        Split tagA tagB -> layout tagA <> D.text "|" <> layout tagB
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
        Newpage -> D.break <> D.break
        Pagebreak _ -> D.break <> D.break
        WithId _ _ tag -> layout tag -- FIXME, implement with Markdown Extensions
        WithClass _ _ tag -> layout tag -- FIXME, implement with Markdown Extensions
        Custom _ _ tag -> layout tag -- FIXME
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


directiveRule :: forall content. (content -> S.Seam) -> X.Directive -> (content -> S.Seam)
directiveRule layoutS = case _ of
    X.Format format ->
        case format of
            X.Fg (E.Left colorStr) -> htmlTagWithStyle "span" $ "color:" <> colorStr
            X.Fg (E.Right color) -> htmlTagWithStyle "span" $ "color:" <> Color.toHexString color
            X.Bg (E.Left colorStr) -> htmlTagWithStyle "span" $ "background-color:" <> colorStr
            X.Bg (E.Right color) -> htmlTagWithStyle "span" $ "background-color:" <> Color.toHexString color
            X.Header hLevel mbAnchor ->
                \content ->
                    (S.markup $ replicate (X.hLevelToInt hLevel) "#") <> S.space <> layoutS content <> case mbAnchor of
                        Just (X.Anchor anchor) -> S.space <> bracketSWith "{#" "}" anchor
                        Nothing -> S.nil
            X.Bold -> wrapWith "**"
            X.Emphasis -> wrapWith "*"
            X.Highlight -> wrapWith "=="
            X.Underline -> wrapWith "__"
            X.Strikethrough -> wrapWith "~~"
            X.Monospaced -> wrapWith "```"
            X.Verbatim -> embraceWith "```"
            X.FixedWidth -> embraceWith "```"
            X.Code (X.ProgrammingLanguage lang) -> surroundWith ("```" <> lang) "```"
            X.Quote mbAuthor ->
                \content ->
                    (S.mark ">" $ layoutS content) <>
                        case mbAuthor of
                            Just (X.QuoteOf author) -> S.space <> (S.mark "--" $ S.text author) -- D.break <> (D.mark ">" $ D.mark "--" $ D.text author)
                            Nothing -> S.nil
                        -- TODO: split by \n and append `>` for every line
            X.Sub -> wrapWith "~"
            X.Sup -> wrapWith "^"
            X.Blink -> htmlTag "blink" []
            X.Inverse -> htmlTag "inverse" []
            X.Invisible -> htmlTagWithStyle "span" "display:none"
            {-
            X.Define (Term dt) ->
                \content -> layout dt <> D.break <> S.mark ":" $ layoutS content
            -}
            X.LinkTo (X.FootnoteId (E.Left ftn))  -> \content -> layoutS content <> S.space <> S.bracket "[^" (S.markup $ show ftn) "]"
            X.LinkTo (X.FootnoteId (E.Right ftn)) -> \content -> layoutS content <> S.space <> S.bracket "[^" (S.markup ftn) "]"
            X.Link (X.Url url) -> \content -> S.bracket "[" (layoutS content) "]" <> S.bracket "(" (S.markup url) ")"
            X.InlineImage (X.ImageParams params) (X.Url url) ->
                case params.width /\ params.height of
                    X.Auto /\ X.Auto -> \content -> S.bracket "![" (layoutS content) "]" <> S.bracket "(" (S.markup url) ")" -- FIXME: use caption for the title
                    X.Px wpx /\ X.Px hpx -> htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ show hpx, "alt" /\ unwrap params.caption, inlineBlockStyle ]
                    X.Px wpx /\ X.Auto ->   htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ "auto", "alt" /\ unwrap params.caption, inlineBlockStyle ]
                    X.Auto /\ X.Px hpx ->   htmlTag "img" [ "src" /\ url, "width" /\ "auto", "height" /\ show hpx, "alt" /\ unwrap params.caption, inlineBlockStyle ]
            X.Comment -> bracketWith "<!--" "-->"
            X.Footnote (X.FootnoteId (E.Left ftn)) ->  \content -> S.bracket "[^" (S.markup $ show ftn) "]" <> S.markup ":" <> S.space <> layoutS content
            X.Footnote (X.FootnoteId (E.Right ftn)) -> \content -> S.bracket "[^" (S.markup ftn)        "]" <> S.markup ":" <> S.space <> layoutS content
    X.Align X.Left   -> htmlTagWithStyle "div" "text-align:left"
    X.Align X.Right  -> htmlTagWithStyle "div" "text-align:right"
    X.Align X.Center -> htmlTagWithStyle "div" "text-align:center"
    X.List part ->
        const S.nil -- FIXME: TODO
        -- D.nest' 0 $ -- FIXME: support levels from `Nest`
        --     [ layout start
        --     , D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout <$> items)
        --     ]
    X.DefList definitions ->
        const S.nil -- FIXME: TODO
        -- D.joinWith (D.break <> D.break) $ def <$> definitions
    X.Table part ->
        const S.nil -- FIXME: TODO
        -- wrap' "table"
        --     $ (wrap' "thead" $ D.stack $ wrap "th" <$> headers)
        --     <> D.break <> D.stack (wrap' "tr" <$> D.stack <$> map (wrap "td") <$> rows)
    X.Image (X.ImageParams params) (X.Url url) ->
        case params.width /\ params.height of
            X.Auto /\ X.Auto ->     \content -> S.bracket "![" (S.text $ unwrap params.caption) "]" <> S.bracket "(" (S.markup url) ")"
            X.Px wpx /\ X.Px hpx -> htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ show hpx, "alt" /\ unwrap params.caption ]
            X.Px wpx /\ X.Auto ->   htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ "auto",   "alt" /\ unwrap params.caption ]
            X.Auto /\ X.Px hpx ->   htmlTag "img" [ "src" /\ url, "width" /\ "auto",   "height" /\ show hpx, "alt" /\ unwrap params.caption ]
    X.Hr ->
        const $ S.markup "---------"
    X.Newpage -> const $ S.break <> S.break
    X.Pagebreak _ -> const $ S.break <> S.break
    X.WithId _ _ -> layoutS -- FIXME, implement with Markdown Extensions
    X.WithClass _ _ -> layoutS-- FIXME, implement with Markdown Extensions
    X.Custom _ _ -> layoutS -- FIXME
    where
        htmlTag name attrs content = S.tag name attrs $ layoutS content
        htmlTagWithStyle name style = htmlTag name [ "style" /\ style ]
        inlineBlockStyle = "style" /\ "display:inline-block"

        wrapWith marker  = S.wrap marker <<< layoutS

        embraceWith marker  = S.embrace marker <<< layoutS

        bracketWith markerL markerR  = S.bracket_ markerL markerR <<< layoutS
        bracketSWith markerL markerR = S.bracket_ markerL markerR <<< S.text

        surroundWith markerL markerR  = S.surround_ markerL markerR <<< layoutS

        replicate n = String.joinWith "" <<< Array.replicate n

        def (TAndD (Term term /\ Definition definition)) = layout term <> D.break <> (D.mark ": " $ layout definition)
        b bullet (index /\ doc) = bulletPrefix index bullet /\ doc



directiveRuleAlt :: X.Directive -> SA.SeamAlt
directiveRuleAlt = case _ of
    X.Format format ->
        case format of
            X.Fg (E.Left colorStr) -> htmlTagWithStyle "span" $ "color:" <> colorStr
            X.Fg (E.Right color) -> htmlTagWithStyle "span" $ "color:" <> Color.toHexString color
            X.Bg (E.Left colorStr) -> htmlTagWithStyle "span" $ "background-color:" <> colorStr
            X.Bg (E.Right color) -> htmlTagWithStyle "span" $ "background-color:" <> Color.toHexString color
            X.Header hLevel mbAnchor ->
                bracketWithSp (replicate (X.hLevelToInt hLevel) "#") $ case mbAnchor of
                        Just (X.Anchor anchor) -> "{#" <> anchor <> "}"
                        Nothing -> ""
            X.Bold -> wrapWith "**"
            X.Emphasis -> wrapWith "*"
            X.Highlight -> wrapWith "=="
            X.Underline -> wrapWith "__"
            X.Strikethrough -> wrapWith "~~"
            X.Monospaced -> wrapWith "```"
            X.Verbatim -> embraceWith "```"
            X.FixedWidth -> embraceWith "```"
            X.Code (X.ProgrammingLanguage lang) -> surroundWith ("```" <> lang) "```"
            X.Quote mbAuthor ->
                bracketWithSp ">" $ case mbAuthor of
                        Just (X.QuoteOf author) -> "-- " <> author
                        Nothing -> ""
            X.Sub -> wrapWith "~"
            X.Sup -> wrapWith "^"
            X.Blink -> htmlTag "blink" []
            X.Inverse -> htmlTag "inverse" []
            X.Invisible -> htmlTagWithStyle "span" "display:none"
            {-
            X.Define (Term dt) ->
                \content -> layout dt <> D.break <> S.mark ":" $ layoutS content
            -}
            X.LinkTo (X.FootnoteId (E.Left ftn))  -> bracketWithSp "" ("[^" <> show ftn <> "]")
            X.LinkTo (X.FootnoteId (E.Right ftn)) -> bracketWithSp "" ("[^" <> ftn <> "]") -- \content -> layoutS content <> S.space <> S.bracket "[^" (S.markup ftn) "]"
            X.Link (X.Url url) -> bracketWith "[" ("](" <> url <> ")")
            X.InlineImage (X.ImageParams params) (X.Url url) ->
                case params.width /\ params.height of
                    X.Auto /\ X.Auto -> bracketWith "![" ("](" <> url <> ")") -- FIXME: use caption for the title
                    X.Px wpx /\ X.Px hpx -> htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ show hpx, "alt" /\ unwrap params.caption, inlineBlockStyle ]
                    X.Px wpx /\ X.Auto ->   htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ "auto", "alt" /\ unwrap params.caption, inlineBlockStyle ]
                    X.Auto /\ X.Px hpx ->   htmlTag "img" [ "src" /\ url, "width" /\ "auto", "height" /\ show hpx, "alt" /\ unwrap params.caption, inlineBlockStyle ]
            X.Comment -> bracketWith "<!--" "-->"
            X.Footnote (X.FootnoteId (E.Left ftn)) ->  bracketWith ("[^" <> show ftn <> "]: ") ""
            X.Footnote (X.FootnoteId (E.Right ftn)) -> bracketWith ("[^" <> ftn  <>    "]: ") ""
    X.Align X.Left   -> htmlTagWithStyle "div" "text-align:left"
    X.Align X.Right  -> htmlTagWithStyle "div" "text-align:right"
    X.Align X.Center -> htmlTagWithStyle "div" "text-align:center"
    X.List part ->
        SA.Nil -- FIXME: TODO
        -- D.nest' 0 $ -- FIXME: support levels from `Nest`
        --     [ layout start
        --     , D.nest' 1 $ uncurry D.mark <$> b bullet <$> Array.mapWithIndex (/\) (layout <$> items)
        --     ]
    X.DefList definitions ->
        SA.Nil -- FIXME: TODO
        -- D.joinWith (D.break <> D.break) $ def <$> definitions
    X.Table part ->
        SA.Nil -- FIXME: TODO
        -- wrap' "table"
        --     $ (wrap' "thead" $ D.stack $ wrap "th" <$> headers)
        --     <> D.break <> D.stack (wrap' "tr" <$> D.stack <$> map (wrap "td") <$> rows)
    X.Image (X.ImageParams params) (X.Url url) ->
        case params.width /\ params.height of
            X.Auto /\ X.Auto ->     bracketWith "![" ("](" <> url <> ")")
            X.Px wpx /\ X.Px hpx -> htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ show hpx, "alt" /\ unwrap params.caption ]
            X.Px wpx /\ X.Auto ->   htmlTag "img" [ "src" /\ url, "width" /\ show wpx, "height" /\ "auto",   "alt" /\ unwrap params.caption ]
            X.Auto /\ X.Px hpx ->   htmlTag "img" [ "src" /\ url, "width" /\ "auto",   "height" /\ show hpx, "alt" /\ unwrap params.caption ]
    X.Hr ->
        single "---------"
    X.Newpage -> single "\n\n" -- FIXME
    X.Pagebreak _ -> single "\n\n" -- FIXME
    X.WithId _ _ -> SA.Nil -- FIXME, implement with Markdown Extensions
    X.WithClass _ _ -> SA.Nil-- FIXME, implement with Markdown Extensions
    X.Custom _ _ -> SA.Nil -- FIXME
    where
        htmlTag name attrs = SA.HtmlTag { name, attrs }
        htmlTagWithStyle name style = htmlTag name [ "style" /\ style ]
        inlineBlockStyle = "style" /\ "display:inline-block"

        single = SA.Single

        wrapWith marker              = SA.SurroundInline { spaced : false, left : marker, right : marker }
        bracketWith markerL markerR  = SA.SurroundInline { spaced : false, left : markerL, right : markerR }

        wrapWithSp marker              = SA.SurroundInline { spaced : true, left : marker, right : marker }
        bracketWithSp markerL markerR  = SA.SurroundInline { spaced : true, left : markerL, right : markerR }

        embraceWith marker        = SA.SurroundBlock { above : marker, below : marker }
        surroundWith above below  = SA.SurroundBlock { above, below }

        replicate n = String.joinWith "" <<< Array.replicate n

        def (TAndD (Term term /\ Definition definition)) = layout term <> D.break <> (D.mark ": " $ layout definition)
        b bullet (index /\ doc) = bulletPrefix index bullet /\ doc