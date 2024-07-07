module Data.Text.Output.Org where

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


foreign import data Org :: OutputKind


org = Proxy :: _ Org


layout :: Tag -> Doc
layout = O.layout org


instance Renderer Org where

    supported :: Proxy Org -> Tag -> Support
    supported _ = case _ of
        _ -> S.Full -- FIXME

    layout :: Proxy Org -> Tag -> Doc
    layout = const $ case _ of
        Empty -> D.nil
        Plain str -> D.text str
        Newline -> D.break
        Format format tag ->
            case format of
                Fg _ -> layout tag
                Bg _ -> layout tag
                Header (Level n) _ ->
                    D.repeat n (D.text "*") <> D.space <> layout tag -- TODO: use anchor
                Bold -> D.wrap "*" $ layout tag
                Emphasis -> D.wrap "/" $ layout tag
                Highlight -> D.wrap "==" $ layout tag
                Underline -> D.wrap "_" $ layout tag
                Strikethrough -> D.wrap "+" $ layout tag
                Monospaced -> D.wrap "~" $ layout tag
                Verbatim -> D.wrap "=" $ layout tag
                FixedWidth -> D.wrapbr "~" $ layout tag
                Code (ProgrammingLanguage lang) -> D.bracketbr ("#+BEGIN_SRC " <> lang) (layout tag) "#+END_SRC"
                Quote -> D.bracketbr "#+BEGIN_QUOTE" (layout tag) "#+END_QUOTE"
                Sub -> D.bracket "^_" (layout tag) "}"
                Sup -> D.bracket "^{" (layout tag) "}"
                Blink -> layout tag
                Inverse -> layout tag
                Invisible -> D.nil
                Define (Term dt) -> layout dt <> D.break <> D.mark ":" (layout tag)
                LinkTo (FootnoteId (E.Left ftn)) -> layout tag <> D.bracket "[^" (D.text $ show ftn) "]"
                LinkTo (FootnoteId (E.Right ftn)) -> layout tag <> D.bracket "[^" (D.text ftn) "]"
                Link (Url url) -> D.bracket "[" (D.bracket "[" (layout tag) "]" <> D.bracket "[" (D.text url) "]") "]"
                Image (Url url) -> D.mark "#+CAPTION:" (layout tag) <> D.break <> D.bracket "[[" (D.text url) "]]"
                Comment -> D.bracketbr "#+BEGIN_COMMENT" (layout tag) "#+END_COMMENT"
                Footnote (FootnoteId (E.Left ftn)) -> D.bracket "[fn:" (D.text $ show ftn) "]" <> D.space <> layout tag
                Footnote (FootnoteId (E.Right ftn)) -> D.bracket "[fn:" (D.text ftn) "]" <> D.space <> layout tag
                -- _ -> layout tag -- other format's are not supported
        Split tagA tagB -> layout tagA <> D.text "|" <> layout tagB
        Align Left tag ->   {- D.mark "#+ALIGN_HTML:" (D.text "left") <> D.break <> -} layout tag
        Align Right tag ->  {- D.mark "#+ALIGN_HTML:" (D.text "right") <> D.break <> -} layout tag
        Align Center tag -> {- D.mark "#+ALIGN_HTML:" (D.text "center") <> D.break <> -} layout tag
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
        Table items -> D.nil -- TODO
        Hr -> D.text "---------"
        where
            b bullet (index /\ doc) = bulletPrefix index bullet /\ doc