module Data.Text.Format.Org.Construct where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (empty) as Map
import Data.Text.Format.Org.Types
import Data.Array.NonEmpty as NEA


newtype PropName = PropName String
newtype PropValue = PropValue String


data Property = Property PropName PropValue


pname = PropName :: String -> PropName
pvalue = PropValue :: String -> PropValue
prop = Property :: PropName -> PropValue -> Property


f :: OrgDoc -> OrgFile
f = f_ []

f_ :: Array Property -> OrgDoc -> OrgFile
f_ _ doc = OrgFile { meta : Map.empty, doc : doc }


d :: Array Section -> OrgDoc
d _ = OrgDoc { zeroth : [], sections : [] }


d' :: Array Block -> OrgDoc
d' _ = OrgDoc { zeroth : [], sections : [] }


d_ :: Array Block -> Array Section -> OrgDoc
d_ _ _ = OrgDoc { zeroth : [], sections : [] }


quote :: String -> Block
quote = Quote


example :: String -> Block
example = Example


code :: String -> Block
code = Code Nothing


list :: ListType -> Array Item -> Block
list _ _ = quote "" -- FIXME


table :: Block
table = quote "" -- FIXME


para :: Array Words -> Block
para _ = quote "" -- FIXME


b :: String -> Words
b = Bold


i :: String -> Words
i = Italic


hl :: String -> Words
hl = Highlight


u :: String -> Words
u = Underline


v :: String -> Words
v = Verbatim


s :: String -> Words
s = Strike


a :: URL -> String -> Words
a url = Link url <<< Just


a' :: URL -> Words
a' url = Link url Nothing


img :: URL -> Words
img = Image


text :: String -> Words
text = Plain


sec :: Int -> Array Words -> OrgDoc -> Section
sec level heading doc =
    Section
        { todo : Nothing
        , priority : Nothing
        , cookie : Nothing
        , check : Nothing
        , heading : NEA.singleton $ b "" -- FIXME
        , level
        , tags : []
        , planning :
            { closed : Nothing
            , deadline : Nothing
            , scheduled : Nothing
            , timestamp : Nothing
            }
        , props : Map.empty
        , doc
        }