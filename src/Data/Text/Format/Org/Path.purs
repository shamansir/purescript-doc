module Data.Text.Format.Org.Path where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Maybe (Maybe(..))

import Data.Text.Format.Org.Types (OrgFile, Block, Section) as T


data Axis a
    = Block a
    | Section a
    | Property a
    | Word a


data Path a
    = Root
    | At (Axis a)
    | Level (Axis a) (Axis a)


findBlock :: forall a. BoundedEnum a => T.OrgFile -> Path a -> Maybe T.Block
findBlock file path = Nothing


findSection :: forall a. BoundedEnum a => T.OrgFile -> Path a -> Maybe T.Section
findSection file path = Nothing


root = Root
