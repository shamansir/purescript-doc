module Data.Text.Format.Org.Construct where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (empty) as Map
import Data.Text.Format.Org.Types
import Data.Text.Format.Org.Path (Path)
import Data.Text.Format.Org.Path as P
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array.NonEmpty as NEA


newtype PropName = PropName String
newtype PropValue = PropValue String


data Property = Property PropName PropValue


pname = PropName :: String -> PropName
pvalue = PropValue :: String -> PropValue
prop = Property :: PropName -> PropValue -> Property


empty :: OrgFile
empty = f emptyDoc


emptyDoc :: OrgDoc
emptyDoc =
    OrgDoc { zeroth : [], sections : [] }


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
        , drawers : []
        , doc
        }


set :: Todo -> Section -> Section
set _ = identity -- TODO


priority :: Priority -> Section -> Section
priority _ = identity -- TODO


low :: Section -> Section
low = identity -- TODO


hi :: Section -> Section
hi = identity -- TODO


cookie :: Cookie -> Section -> Section
cookie _ = identity -- TODO


tag :: String -> Section -> Section
tag _ = identity -- TODO


untag :: String -> Section -> Section
untag _ = identity -- TODO


level :: Int -> Section -> Section
level _ = identity -- TODO


inc :: Section -> Section
inc = identity -- TODO


dec :: Section -> Section
dec = identity -- TODO


close :: OrgDateTime -> Section -> Section
close _ = identity -- TODO


deadline :: OrgDateTime -> Section -> Section
deadline _ = identity -- TODO


schedule :: OrgDateTime -> Section -> Section
schedule _ = identity -- TODO


timestamp :: OrgDateTime -> Section -> Section
timestamp _ = identity -- TODO


wprop :: Property -> Section -> Section
wprop _ = identity


drawer :: Drawer -> Section -> Section
drawer _ = identity


note :: String -> Section -> Section
note _ = identity -- LOGBOOK


addSection :: Path -> OrgFile -> Section -> Path /\ OrgFile
addSection where_ file _ = P.empty /\ file


addBlock :: Path -> OrgFile -> Block -> Path /\ OrgFile
addBlock where_ file _ = P.empty /\ file


addSection' :: OrgFile -> Section -> Path /\ OrgFile
addSection' = addSection P.empty


addBlock' :: OrgFile -> Block -> Path /\ OrgFile
addBlock' = addBlock P.empty