module Data.Text.Format.Org.Render where

import Prelude

import Data.Newtype (unwrap)

import Data.Text.Format.Org.Types (OrgFile(..))
import Data.Text.Format.Org.Types as Org


import Data.Text.Doc (Doc, (<+>), (</>))
import Data.Text.Doc as D


layout :: OrgFile -> Doc
layout (OrgFile { meta, doc }) =
    D.wrap "foo" $ D.text "aaa"