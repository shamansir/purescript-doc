module Data.Text.Format.Org.Construct where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (empty) as Map
import Data.Text.Format.Org.Types


newtype PropName = PropName String
newtype PropValue = PropValue String


data Property = Property PropName PropValue


pname = PropName :: String -> PropName
pvalue = PropValue :: String -> PropValue
prop = Property :: PropName -> PropValue -> Property


data DocItem
    = B Block
    | S Section
    | P DocItem DocItem


f :: Array Property -> OrgDoc -> OrgFile
f _ doc = OrgFile { meta : Map.empty, doc : doc }


d :: DocItem -> OrgDoc
d _ = OrgDoc { blocks : [], sections : [] }


quote :: String -> DocItem
quote = B <<< Quote


example :: String -> DocItem
example = B <<< Example


code :: String -> DocItem
code = B <<< Code Nothing


list :: ListType -> Array Item -> DocItem
list _ _ = quote "" -- FIXME


text :: String -> Words
text = Plain