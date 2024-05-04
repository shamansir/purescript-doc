module Data.Text.Format.Org.Render where

import Prelude

import Data.Newtype (unwrap)
import Data.Map as Map
import Data.Array as Array
import Data.Array.NonEmpty as NEA
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
layoutDoc indent (OrgDoc { zeroth, sections }) =
    sections # map (layoutSection indent) # Array.foldl (<>) D.nil


layoutBlock :: Int -> Org.Block -> Doc
layoutBlock indent block =
    D.nil


layoutSection :: Int -> Org.Section -> Doc
layoutSection indent (Org.Section section) =
    let
        headingLine =
            section.heading # NEA.toArray # map layoutWords # Array.foldl (<>) D.nil
    in
        if not $ Org.isDocEmpty section.doc then
            headingLine </> layoutDoc indent section.doc
        else
            headingLine


layoutWords :: Org.Words -> Doc
layoutWords = case _ of
    Org.Plain plain -> D.text plain
    _ -> D.nil