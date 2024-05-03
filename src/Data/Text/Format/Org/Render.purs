module Data.Text.Format.Org.Render where

import Prelude

import Data.Newtype (unwrap)
import Data.Map as Map
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))

import Data.Text.Format.Org.Types (OrgFile(..), OrgDoc(..))
import Data.Text.Format.Org.Types as Org
import Data.Text.Format.Org.Construct as Org


import Data.Text.Doc (Doc, (<+>), (</>), (<//>))
import Data.Text.Doc as D


layout :: OrgFile -> Doc
layout (OrgFile { meta, doc }) =
    case (Map.isEmpty meta /\ Org.isDocEmpty doc) of
        (true /\ true) -> D.nil
        (false /\ true) -> meta # Map.toUnfoldable # map renderMeta # D.stack
        (true /\ false) -> layoutDoc 0 doc
        (false /\ false) ->
            (meta # Map.toUnfoldable # map renderMeta # D.stack)
            <//> layoutDoc 0 doc
    where
        renderMeta ((idx /\ key) /\ value) =
            D.bracket "#+" (D.text key) ":" <+> D.text value


layoutDoc :: Int -> OrgDoc -> Doc
layoutDoc indent doc =
    D.nil