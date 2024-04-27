module Data.Text.Format.Org.Path where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

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


root :: forall a. Path a
root = Root


instance Show a => Show (Axis a) where
    show = case _ of
        Block a -> "B" <> show a
        Section a -> "S" <> show a
        Property a -> "P" <> show a
        Word a -> "W" <> show a


instance Show a => Show (Path a) where
    show = case _ of
        Root -> "*"
        At axis -> show axis
        Level axisA axisB -> show axisA <> " -> " <> show axisB