module Data.Text.Output where

import Prelude

import Color as Color
import Data.Maybe (Maybe (..))
import Data.String as String

import Type.Proxy (Proxy)

import Data.Text.Format (Tag(..), class Formatter, format)
import Data.Text.Doc (Doc)
import Data.Text.Doc (Doc(..), Options, render) as Doc


data OutputKind


-- foreign import data Blessed :: OutputKind
-- foreign import data OneLine :: OutputKind
-- foreign import data PlainText :: OutputKind
-- foreign import data Markdown :: OutputKind
-- foreign import data Html :: OutputKind


data Support
    = Full
    | Partly -- renders improperly but tries to apply formatting
    | Text -- just renders plain text it finds inside
    | None -- not even renders


class ToDoc a where
    toDoc :: a -> Doc


class FormatterBy (x :: OutputKind) a where
    formatBy :: Proxy x -> a -> Tag


class Renderer (x :: OutputKind) where
    -- options :: Proxy x -> Options
    supported :: Proxy x -> Tag -> Support
    layout :: Proxy x -> Tag -> Doc


class Renderer x <= Format (x :: OutputKind) a | x -> a where
    perform :: Proxy x -> Doc.Options -> a -> String


instance (Renderer x, Formatter a) => Format x a where
    perform p opts = format >>> layout p >>> Doc.render opts
else instance (Renderer x, FormatterBy x a) => Format x a where
    perform p opts = formatBy p >>> layout p >>> Doc.render opts