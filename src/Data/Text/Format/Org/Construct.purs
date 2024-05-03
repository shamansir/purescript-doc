module Data.Text.Format.Org.Construct where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (empty, insert, size) as Map
import Data.Enum (class BoundedEnum)
import Data.Tuple (curry, uncurry)
import Data.Foldable (class Foldable)
import Data.Unfoldable (class Unfoldable)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (toUnfoldable, length) as Array
import Data.Array.NonEmpty as NEA


import Data.Text.Format.Org.Types
import Data.Text.Format.Org.Path (Path)
import Data.Text.Format.Org.Path as P


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


ds :: Array Section -> OrgDoc
ds _ = OrgDoc { zeroth : [], sections : [] }


db :: Array Block -> OrgDoc
db _ = OrgDoc { zeroth : [], sections : [] }


dbs :: Array Block -> Array Section -> OrgDoc
dbs _ _ = OrgDoc { zeroth : [], sections : [] }


meta :: String -> String -> OrgFile -> OrgFile
meta prop val (OrgFile { meta, doc }) =
    OrgFile
        { meta : meta # Map.insert (Map.size meta /\ prop) val
        , doc : doc
        }


metan :: Int -> String -> String -> OrgFile -> OrgFile
metan n prop val (OrgFile { meta, doc }) =
    OrgFile
        { meta : meta # Map.insert (n /\ prop) val
        , doc : doc
        }


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
            Planning
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


{-
data At
    = AtMeta String String
    | AtBlock Block
    | AtWords Words
    | AtSection Section
    | AtHeading Words
    | AtProperty String String
    | AtTag String String
    | AtDrawer Drawer
    | AtPriority Priority
    | AtPlanning -- TODO


type Cursor a =
    { path :: Path a
    , parent :: Maybe At
    , current :: At
    }


mapTraverse :: forall a x z. (x -> x -> z) -> (Cursor a -> x) -> x -> OrgFile -> Array z
mapTraverse = ?wh


mapTraverse' :: forall a x z. (x -> x -> z) -> (Cursor a -> x) -> x -> OrgDoc -> Array z
mapTraverse' = ?wh
-}

-- data At :: forall k. (k -> Type) -> k -> Type
data At :: (Type -> Type) -> Type -> Type
data At f a
    = AtBlock Block
    | AtSection Section (f a)


-- traverse ∷ ∀ (x ∷ Type) (b ∷ Type) (a ∷ x) (f ∷ x -> Type). (Array b → Array b → f a) → (At f a → b) → OrgDoc → f a
traverse :: forall b a (f ∷ Type -> Type). Unfoldable f => (f b → f b → f a) -> (At f a -> b) -> OrgDoc -> f a
traverse join f (OrgDoc doc) =
    join (Array.toUnfoldable $ map (f <<< AtBlock) doc.zeroth) (Array.toUnfoldable $ map (f <<< uncurry AtSection <<< deepF) doc.sections)
    where deepF (Section sec) = Section sec /\ traverse join f sec.doc


findBlock :: forall a. BoundedEnum a => OrgFile -> Path a -> Maybe Block
findBlock file path = Nothing


findSection :: forall a. BoundedEnum a => OrgFile -> Path a -> Maybe Section
findSection file path = Nothing


addSection :: forall a. Path a -> OrgFile -> Section -> Path a /\ OrgFile
addSection where_ file _ =
    {- case where_ of
        Root -> -} P.root /\ file


addBlock :: forall a. Path a -> OrgFile -> Block -> Path a /\ OrgFile
addBlock where_ file _ = P.root /\ file


addSection' :: forall a. OrgFile -> Section -> Path a /\ OrgFile
addSection' = addSection P.root


addBlock' :: forall a. OrgFile -> Block -> Path a /\ OrgFile
addBlock' = addBlock P.root


isDocEmpty :: OrgDoc -> Boolean
isDocEmpty (OrgDoc { zeroth, sections }) =
    Array.length zeroth == 0 && Array.length sections == 0