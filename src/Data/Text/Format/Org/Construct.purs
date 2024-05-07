module Data.Text.Format.Org.Construct where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (empty, insert, size, fromFoldable) as Map
import Data.Enum (class BoundedEnum)
import Data.Tuple (curry, uncurry)
import Data.Foldable (class Foldable)
import Data.Unfoldable (class Unfoldable)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (toUnfoldable, length, mapWithIndex, singleton, delete) as Array
import Data.Array.NonEmpty as NEA
import Data.String (joinWith, toUpper) as String
import Data.Newtype (unwrap, wrap)


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
f_ props doc =
    OrgFile { meta : Map.fromFoldable $ Array.mapWithIndex extractProp props, doc }
    where
    extractProp idx (Property (PropName name) (PropValue value)) = (idx /\ name) /\ value


ds :: Array Section -> OrgDoc
ds sections = OrgDoc { zeroth : [], sections }


ds1 :: Section -> OrgDoc
ds1 = ds <<< Array.singleton


db :: Array Block -> OrgDoc
db blocks = OrgDoc { zeroth : blocks, sections : [] }


db1 :: Block -> OrgDoc
db1 = db <<< Array.singleton


dbs :: Array Block -> Array Section -> OrgDoc
dbs blocks sections = OrgDoc { zeroth : blocks, sections }


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


data ProgressStep = Progress String
data FinishStep = Finish String


todoSequence :: Array ProgressStep -> Array FinishStep -> OrgFile -> OrgFile
todoSequence pss fss =
    meta "SEQ_TODO" $ String.joinWith " " (pssToString <$> pss) <> " | " <> String.joinWith " " (fssToString <$> fss)
    where
     pssToString (Progress str) = String.toUpper str
     fssToString (Finish str) = String.toUpper str


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
para = Paragraph <<< fromMaybe (NEA.singleton $ Plain "\n") <<< NEA.fromArray


para1 :: Words -> Block
para1 = Paragraph <<< NEA.singleton


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


br :: Words
br = Break


sec :: Int -> Array Words -> OrgDoc -> Section
sec level heading doc =
    Section
        { todo : Nothing
        , priority : Nothing
        , cookie : Nothing
        , check : Nothing
        , heading : NEA.fromArray heading # fromMaybe (NEA.singleton $ text "")
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
        , comment : false
        , doc
        }


sec1 :: Int -> Words -> OrgDoc -> Section
sec1 l = sec l <<< Array.singleton


sece :: Int -> Array Words -> Section
sece l ws = sec l ws emptyDoc


sece1 :: Int -> Words -> Section
sece1 l = sece l <<< Array.singleton


ssec :: Int -> Array Words -> OrgDoc -> OrgDoc
ssec level heading doc =
    ds [ sec level heading doc ]


ssec1 :: Int -> Words -> OrgDoc -> OrgDoc    
ssec1 l = ssec l <<< Array.singleton


__qset f (Section sec) = Section $ f sec --  unwrap >>> f >> wrap


__qplan f =
    __qset $ \sec -> sec { planning = Planning $ f $ case sec.planning of Planning p -> f p }


set :: Todo -> Section -> Section
set val = __qset _ { todo = Just val }


priority :: Priority -> Section -> Section
priority val = __qset _ { priority = Just val }


low :: Section -> Section
low = identity -- TODO


hi :: Section -> Section
hi = identity -- TODO


cookie :: Cookie -> Section -> Section
cookie val = __qset _ { cookie = Just val }


tag :: String -> Section -> Section
tag s = __qset $ \sec -> sec { tags = s : sec.tags }


untag :: String -> Section -> Section
untag s = __qset $ \sec -> sec { tags = sec.tags # Array.delete s }


level :: Int -> Section -> Section
level val = __qset _ { level = val }


inc :: Section -> Section
inc = __qset $ \sec -> sec { level = min 20 $ sec.level + 1 }


dec :: Section -> Section
dec = __qset $ \sec -> sec { level = max 0 $ sec.level - 1 }


close :: OrgDateTime -> Section -> Section
close dt = __qplan $ _ { closed = Just dt }


deadline :: OrgDateTime -> Section -> Section
deadline dt = __qplan $ _ { deadline = Just dt }


schedule :: OrgDateTime -> Section -> Section
schedule dt = __qplan $ _ { scheduled = Just dt } -- TODO


timestamp :: OrgDateTime -> Section -> Section
timestamp dt = __qplan $ _ { timestamp = Just dt } -- TODO -- TODO


wprop :: Property -> Section -> Section
wprop _ = identity -- TODO


drawer :: Drawer -> Section -> Section
drawer _ = identity -- TODO


note :: String -> Section -> Section
note _ = identity  -- TODO -- LOGBOOK


comment :: Section -> Section
comment = __qset _ { comment = true }


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