module Data.Text.Format.Dodo.Tag where

import Prelude

import Color (Color)
import Data.Array (singleton, (:))
import Data.Either (Either(..)) as E
import Data.Either (Either)
import Data.Bounded (top, bottom)
import Data.Enum (class Enum, fromEnum, toEnum, pred, succ)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.FunctorWithIndex (mapWithIndex)

import Dodo (Doc, annotate) as Dodo
import Dodo (text, space, break, enclose, foldWith, foldWithSeparator, lines, paragraph, indent) as D


type Tag = Dodo.Doc Directive -- it is called `Tag` For backward compatibility


_tag :: Directive -> Tag -> Tag
_tag = Dodo.annotate


_etag :: Directive -> Tag
_etag t = _tag t $ mempty


newtype Indent = Indent Int
newtype FootnoteId = FootnoteId (Either Int String)
newtype Anchor = Anchor String
newtype ProgrammingLanguage = ProgrammingLanguage String
newtype Url = Url String
newtype Term = Term Tag
newtype Definition = Definition Tag
newtype TermAndDefinition = TAndD (Term /\ Definition)
newtype Caption = Caption String
newtype ImageParams =
        ImageParams
            { width :: ImageSide
            , height :: ImageSide
            , caption :: Caption
            }
newtype QuoteOf = QuoteOf String


derive instance Newtype Indent _
derive instance Newtype FootnoteId _
derive instance Newtype Anchor _
derive instance Newtype ProgrammingLanguage _
derive instance Newtype Url _
derive instance Newtype Term _
derive instance Newtype Definition _
derive instance Newtype TermAndDefinition _
derive instance Newtype Caption _
derive instance Newtype ImageParams _
derive instance Newtype QuoteOf _


derive newtype instance Show Indent
derive newtype instance Show FootnoteId
derive newtype instance Show Anchor
derive newtype instance Show ProgrammingLanguage
-- derive newtype instance Show Term
-- derive newtype instance Show Definition
derive newtype instance Show Caption
derive newtype instance Show QuoteOf

derive instance Eq HLevel
derive instance Ord HLevel


data Align
    = Left
    | Right
    | Center


data HLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


data Format
    = Bold
    | Emphasis -- TODO: Emphasis & Italic could be different i.e. in LaTeX
    | Underline
    | Highlight
    | Blink
    | Inverse
    | Invisible
    | Strikethrough
    | Monospaced
    | Header HLevel (Maybe Anchor)
    | Quote (Maybe QuoteOf)
    | Verbatim
    | Link Url
    | InlineImage ImageParams Url
    | Footnote FootnoteId
    | LinkTo FootnoteId
    | Code ProgrammingLanguage
    | Comment
    | FixedWidth
    | Sub
    | Sup
    | Fg (Either String Color)
    | Bg (Either String Color)
    -- TODO: Resize Smaller / Larger / Scale Number / Exact Px


-- data ListKind
--     = Ordered TheWay
--     | Unordered


data Bullet
    = None
    | Disc
    | Asterisk
    | Dash
    | Num
    | Circle
    | Alpha
    | AlphaInv
    | BCustom String
    -- ..


newtype ChunkId = ChunkId String
newtype ChunkClass = ChunkClass String


data WrapKind
    = Block
    | Inline


data TablePart
    = TRoot
    | THeaderRow
    | THeaderCell
    | THeaderSplit
    | TRow
    | TCell
    | TRowSplit
    | TCellSplit


data ListPart
    = LRoot
    | LHeader
    | LItem Bullet Int


data DefListPart
    = DLRoot
    | DLTerm Int
    | DLDefinition Int


data Directive
    = Format Format
    | Align Align
    | Image ImageParams Url
    | List ListPart
    | DefList DefListPart
    | Table TablePart
    | WithId WrapKind ChunkId
    | WithClass WrapKind ChunkClass
    | Hr
    | Newpage
    | Pagebreak (Maybe Int)
    | Custom String (Array (String /\ String))
    -- TODO: comment block


data ImageSide -- FIXME: reuse some other type
    = Auto
    | Px Int


hr :: Tag
hr = _etag Hr


nil :: Tag
nil = mempty


s :: String -> Tag
s = plain


plain :: String -> Tag
plain = D.text -- TODO: preprocess for newlines and spaces first?


lefts :: String -> Tag
lefts = left <<< plain


left :: Tag -> Tag
left = _tag $ Align Left


rights :: String -> Tag
rights = right <<< plain


right :: Tag -> Tag
right = _tag $ Align Right


centers :: String -> Tag
centers = center <<< plain


center :: Tag -> Tag
center = _tag $ Align Center


bolds :: String -> Tag
bolds = bold <<< plain


bold :: Tag -> Tag
bold = _tag $ Format Bold


em :: Tag -> Tag
em = _tag $ Format Emphasis


i :: Tag -> Tag
i = _tag $ Format Emphasis


u :: Tag -> Tag
u = _tag $ Format Underline


thru :: Tag -> Tag
thru = _tag $ Format Strikethrough


monos :: String -> Tag
monos = mono <<< plain


mono :: Tag -> Tag
mono = _tag $ Format Monospaced


underlines :: String -> Tag
underlines = underline <<< plain


underline :: Tag -> Tag
underline = _tag $ Format Underline


blinks :: String -> Tag
blinks = blink <<< plain


blink :: Tag -> Tag
blink = _tag $ Format Blink


inverses :: String -> Tag
inverses = inverse <<< plain


inverse :: Tag -> Tag
inverse = _tag $ Format Inverse


invisibles :: String -> Tag
invisibles = invisible <<< plain


invisible :: Tag -> Tag
invisible = _tag $ Format Invisible



wrap :: Tag -> Tag -> Tag -> Tag
wrap = D.enclose


wraps :: String -> String -> Tag -> Tag
wraps l r = wrap (plain l) (plain r)


url :: String -> Url
url = Url


link :: Url -> Tag -> Tag
link = _tag <<< Format <<< Link


icaption :: String -> Caption
icaption = Caption


auto_ip :: ImageParams
auto_ip = ImageParams { width : Auto, height : Auto, caption : Caption "" }


img :: Url -> Tag
img = imgp auto_ip


imgc :: Caption -> Url -> Tag
imgc = imgp <<< cauto


imgp :: ImageParams -> Url -> Tag
imgp par = _etag <<< Image par


iimg :: Url -> Tag -> Tag
iimg = iimgp auto_ip


iimgc :: Caption -> Url -> Tag -> Tag
iimgc caption = iimgp $ ImageParams { width : Auto, height : Auto, caption  }


iimgp :: ImageParams -> Url -> Tag -> Tag
iimgp par = _tag <<< Format <<< InlineImage par


sized :: { w :: Int, h :: Int } -> ImageParams
sized = csized $ Caption ""


csized :: Caption -> { w :: Int, h :: Int } -> ImageParams
csized caption { w, h } = ImageParams { width : Px w, height : Px h, caption }


cauto :: Caption -> ImageParams
cauto caption = ImageParams { width : Auto, height : Auto, caption }


ftn :: String -> Tag -> Tag
ftn = _tag <<< Format <<< Footnote <<< FootnoteId <<< E.Right


ftni :: Int -> Tag -> Tag
ftni = _tag <<< Format <<< Footnote <<< FootnoteId <<< E.Left


to_ftn :: String -> Tag -> Tag
to_ftn = _tag <<< Format <<< LinkTo <<< FootnoteId <<< E.Right


to_ftni :: Int -> Tag -> Tag
to_ftni = _tag <<< Format <<< LinkTo <<< FootnoteId <<< E.Left


h :: Int -> Tag -> Tag
h lvl = _tag <<< Format $ Header (hLevelFromInt lvl) Nothing


h' :: Int -> String -> Tag -> Tag
h' lvl = _tag <<< Format <<< Header (hLevelFromInt lvl) <<< Just <<< Anchor


h1 = h 1 :: Tag -> Tag
h2 = h 2 :: Tag -> Tag
h3 = h 3 :: Tag -> Tag
h4 = h 4 :: Tag -> Tag
h5 = h 5 :: Tag -> Tag
h6 = h 6 :: Tag -> Tag


h1' = h' 1 :: String -> Tag -> Tag
h2' = h' 2 :: String -> Tag -> Tag
h3' = h' 3 :: String -> Tag -> Tag
h4' = h' 4 :: String -> Tag -> Tag
h5' = h' 5 :: String -> Tag -> Tag
h6' = h' 6 :: String -> Tag -> Tag



define :: String -> Tag -> Tag -- FIXME: should it be singleton or helper for constructing lists?
define term def = dl [ TAndD $ Term (plain term) /\ Definition def ]


dt :: Tag -> Tag -> TermAndDefinition
dt term def = TAndD $ Term term /\ Definition def


dl :: Array TermAndDefinition -> Tag
dl items = _tag (DefList DLRoot)
        $ D.lines
        $ (D.foldWith (<>) <$> mapWithIndex defineInList items)
    where
        defineInList idx (TAndD (Term term /\ Definition def)) =
            [ _tag (DefList $ DLTerm idx) term
            , _tag (DefList $ DLDefinition idx) def
            ]


f :: Format -> Tag -> Tag
f = _tag <<< Format


none = None :: Bullet
disk = Disc :: Bullet
asterisk = Asterisk :: Bullet
dash = Dash :: Bullet
num = Num :: Bullet
alpha = Alpha :: Bullet
circle = Circle :: Bullet
nalpha = AlphaInv :: Bullet
bcustom = BCustom :: String -> Bullet



list :: Tag -> Array Tag -> Tag
list = listb Dash


list_ :: Array Tag -> Tag
list_ = listb_ Dash


listb :: Bullet -> Tag -> Array Tag -> Tag
listb bullet = _list bullet <<< Just


listb_ :: Bullet -> Array Tag -> Tag
listb_ bul = _list bul Nothing


_list :: Bullet -> Maybe Tag -> Array Tag -> Tag
_list bullet mbHeader items =
    _tag (List LRoot)
        $ D.lines
        $ case mbHeader of
            Just header ->
                _tag (List LHeader) header
                : mapWithIndex (_tag <<< List <<< LItem bullet) items
            Nothing ->
                mapWithIndex (_tag <<< List <<< LItem bullet) items


stack :: Array Tag -> Tag
stack = D.lines


paras :: Array Tag -> Tag
paras = D.paragraph -- Or `D.lines`?


fgcs :: Color -> String -> Tag
fgcs c = fgc c <<< plain


fgc :: Color -> Tag -> Tag
fgc = _tag <<< Format <<< Fg <<< E.Right


fgs :: String -> String -> Tag
fgs cs = fg cs <<< plain


fg :: String -> Tag -> Tag
fg = _tag <<< Format <<< Fg <<< E.Left


bgcs :: Color -> String -> Tag
bgcs c = bgc c <<< plain


bgc :: Color -> Tag -> Tag
bgc = _tag <<< Format <<< Bg <<< E.Right


bgs :: String -> String -> Tag
bgs cs = bg cs <<< plain


bg :: String -> Tag -> Tag
bg = _tag <<< Format <<< Bg <<< E.Left


nl :: Tag
nl = D.break


nest :: Int -> Array Tag -> Tag
nest n = indent n <<< D.lines


indent :: Int -> Tag -> Tag
indent 0 = identity
indent 1 = D.indent
indent n | n < 0 = identity
indent n | otherwise = indent $ n - 1


group :: Array Tag -> Tag
group = D.lines


joinWith :: Tag -> Array Tag -> Tag
joinWith = D.foldWithSeparator


code :: String -> String -> Tag
code pl = _tag (Format $ Code $ ProgrammingLanguage pl) <<< plain


tableh :: Array Tag -> Array (Array Tag) -> Tag
tableh hcells rows =
    _tag (Table TRoot)
    $ D.lines
        $ ( _tag (Table THeaderRow)
            $ joinWith (_etag $ Table THeaderSplit)
                $ (_tag $ Table THeaderCell)
                <$> hcells
            )
        : (_tag (Table TRow)
            <$> (joinWith (_etag $ Table TCellSplit)
                <<< map (_tag $ Table TCell))
                <$> rows
            )


table :: Array (Array Tag) -> Tag
table = tableh []


quote :: Tag -> Tag
quote = _tag $ Format $ Quote Nothing


quote_by :: QuoteOf -> Tag -> Tag
quote_by = _tag <<< Format <<< Quote <<< Just


of_ :: String -> QuoteOf
of_ = QuoteOf


space :: Tag
space = D.space


mark :: Tag -> Tag -> Tag
mark m c = m <> space <> c


sub :: Tag -> Tag
sub = _tag $ Format $ Sub


sup :: Tag -> Tag
sup = _tag $ Format $ Sup


newpage :: Tag
newpage = _etag Newpage


pagebreak :: Tag
pagebreak = _etag $ Pagebreak Nothing


pagebreakAt :: Int -> Tag
pagebreakAt = _etag <<< Pagebreak <<< Just


id :: String -> Tag -> Tag
id = _tag <<< WithId Inline <<< ChunkId


_class :: String -> Tag -> Tag
_class = _tag <<< WithClass Inline <<< ChunkClass


bl_id :: String -> Tag -> Tag
bl_id = _tag <<< WithId Block <<< ChunkId


bl_class :: String -> Tag -> Tag
bl_class = _tag <<< WithClass Block <<< ChunkClass


custom :: String -> Tag -> Tag
custom name = _tag $ Custom name []


custom_ :: String -> Array (String /\ String) -> Tag -> Tag
custom_ name args = _tag $ Custom name args


_null :: Tag -> Tag
_null = identity -- to mark some tag with a plan to replace it with another one in future


{-
-- TODO: also convert to tree
traverse :: (Tag -> Tag) -> Tag -> Tag
traverse f = case _ of
    Format format tag -> f $ Format format $ traverse f tag
    Align align tag -> f $ Align align $ traverse f tag
    Split tagA tagB -> f $ Split (traverse f tagA) (traverse f tagB)
    Pair tagA tagB -> f $ Pair (traverse f tagA) (traverse f tagB)
    Join with tags -> f $ Join (traverse f with) $ traverse f <$> tags
    Wrap tagA tagB tagC -> f $ Wrap (traverse f tagA) (traverse f tagB) (traverse f tagC)
    Para tags -> f $ Para $ traverse f <$> tags
    Nest indent tags -> f $ Nest indent $ traverse f <$> tags
    List bullet hTag tags -> f $ List bullet (traverse f hTag) $ traverse f <$> tags
    Table headers cells -> f $ Table (traverse f <$> headers) (map (traverse f) <$> cells)
    DefList defs -> f $ DefList $ traverseDef <$> defs
    Image params url -> f $ Image params url
    Plain str -> f $ Plain str
    Newline -> f Newline
    Empty -> f Empty
    Hr -> f Hr
    Newpage -> f Newpage
    Pagebreak n -> f $ Pagebreak n
    WithId wk id tag -> f $ WithId wk id $ traverse f tag
    WithClass wk cl tag -> f $ WithClass wk cl $ traverse f tag
    Custom name args tag -> f $ Custom name args $ f tag
    where
        traverseDef :: TermAndDefinition -> TermAndDefinition
        traverseDef (TAndD (Term termTag /\ Definition defTag)) =
             TAndD $ Term (traverse f termTag) /\ Definition (traverse f defTag)



levelUp :: Tag -> Tag
levelUp = traverse $ case _ of
    Format (Header level anchor) tag -> Format (Header (fromMaybe top $ succ level) anchor) tag
    tag -> tag


levelDown :: Tag -> Tag
levelDown = traverse $ case _ of
    Format (Header level anchor) tag -> Format (Header (fromMaybe bottom $ pred level) anchor) tag
    tag -> tag
-}


-- TODO: indent left / right


blank :: Tag
blank = nl <> nl


class Formatter a where
    format :: a -> Tag


instance Formatter Tag where
    format = identity


bulletPrefix :: Int -> Bullet -> String
bulletPrefix index = case _ of
    None -> ""
    Disc -> "•"
    Asterisk -> "*"
    Dash -> "-"
    Circle -> "o" -- FIXME
    Alpha ->    String.singleton (fromMaybe 'a' $ toEnum $ fromEnum 'a' + index) <> "."
    AlphaInv -> String.singleton (fromMaybe 'z' $ toEnum $ fromEnum 'z' - index) <> "."
    Num ->      String.singleton (fromMaybe '1' $ toEnum $ fromEnum '1' + index) <> "."
    BCustom str -> str


{-
disc	Default value. The marker is a filled circle
armenian	The marker is traditional Armenian numbering
circle	The marker is a circle
cjk-ideographic	The marker is plain ideographic numbers
decimal	The marker is a number
decimal-leading-zero	The marker is a number with leading zeros (01, 02, 03, etc.)
georgian	The marker is traditional Georgian numbering
hebrew	The marker is traditional Hebrew numbering
hiragana	The marker is traditional Hiragana numbering
hiragana-iroha	The marker is traditional Hiragana iroha numbering
katakana	The marker is traditional Katakana numbering
katakana-iroha	The marker is traditional Katakana iroha numbering
lower-alpha	The marker is lower-alpha (a, b, c, d, e, etc.)
lower-greek	The marker is lower-greek
lower-latin	The marker is lower-latin (a, b, c, d, e, etc.)
lower-roman	The marker is lower-roman (i, ii, iii, iv, v, etc.)
none	No marker is shown
square	The marker is a square
upper-alpha	The marker is upper-alpha (A, B, C, D, E, etc.)
upper-greek	The marker is upper-greek
upper-latin	The marker is upper-latin (A, B, C, D, E, etc.)
upper-roman	The marker is upper-roman (I, II, III, IV, V, etc.)
-}


instance Bounded HLevel where
    top = H1
    bottom = H6


instance Enum HLevel where
    succ = case _ of
        H1 -> Nothing
        H2 -> Just H1
        H3 -> Just H2
        H4 -> Just H3
        H5 -> Just H4
        H6 -> Just H5
    pred = case _ of
        H1 -> Just H2
        H2 -> Just H3
        H3 -> Just H4
        H4 -> Just H5
        H5 -> Just H6
        H6 -> Nothing


hLevelToInt :: HLevel -> Int
hLevelToInt = case _ of
    H1 -> 1
    H2 -> 2
    H3 -> 3
    H4 -> 4
    H5 -> 5
    H6 -> 6


hLevelFromInt :: Int -> HLevel
hLevelFromInt = case _ of
    0 -> H1
    1 -> H1
    2 -> H2
    3 -> H3
    4 -> H4
    5 -> H5
    6 -> H6
    n -> if n < 0 then top else bottom


-- TODO: do syntax?

instance Show WrapKind where
    show = case _ of
        Block -> "block"
        Inline -> "inline"

{-
instance Show Tag where
    show = case _ of
        Empty -> just "empty"
        Plain str -> wrap "plain" str
        Align align tag -> wraparg "align" (show align) $ show tag
        Format format tag -> case format of
            Fg (E.Left colorStr) -> wraparg "fg" (show colorStr) $ show tag
            Fg (E.Right color) -> wraparg "fg" (show color) $ show tag
            Bg (E.Left colorStr) -> wraparg "bg" (show colorStr) $ show tag
            Bg (E.Right color) -> wraparg "bg" (show color) $ show tag
            Link (Url url) -> wraptag2 "link" (show tag) url
            LinkTo (FootnoteId ftnId) ->
                case ftnId of
                    E.Left ftnIntId -> wraptag2 "link" (show tag) $ wrap "ftn#" $ show ftnIntId
                    E.Right ftnStrId -> wraptag2 "link" (show tag) $ wrap "ftn" $ ftnStrId
            Define (Term term) -> let definition = tag in wraptag2 "def" (show term) $ show definition
            Comment -> wrap "comment" $ show tag
            Footnote (FootnoteId ftnId) ->
                case ftnId of
                    E.Left ftnIntId -> wraparg "footnote" ("#" <> show ftnIntId) $ show tag
                    E.Right ftnStrId -> wraparg "footnote" ftnStrId $ show tag
            InlineImage imgParams (Url url) -> let title = tag in wraparg2 "image" (show title) (show imgParams) $ show url
            _ -> wraparg "format" (show format) $ show tag
        Split tag1 tag2 -> wraptag2 "split" (show tag1) $ show tag2
        Pair tag1 tag2 -> wraptag2 "pair" (show tag1) $ show tag2
        Para tags -> wraplist "para" $ show <$> tags
        Newline -> just "nl"
        Hr -> just "hr"
        Newpage -> just "newpage"
        Pagebreak mbPriority -> case mbPriority of
            Just n -> wrap "pagebreak" $ show n
            Nothing -> just "pagebreak"
        Nest indent tags -> wraplistarg "nest" (show indent) $ show <$> tags
        Join tag tags -> wraplistarg "join" (show tag) $ show <$> tags
        Wrap start end tag -> wraparg2 "wrap" (show start) (show end) $ show tag
        Image imgParams (Url url) -> wraparg "image" (show imgParams) $ show url
        List bullet tag tags -> wraplistarg2 "list" (show bullet) (show tag) $ show <$> tags
        DefList definitions -> wraplist "list" $ show <$> definitions
        Table headers rows -> wrap "table" $ (wrap "header" $ String.joinWith "," $ show <$> headers) <> "|" <> (wraplist "row" $ wraplist "column" <$> map show <$> rows)
        WithId wrapKind (ChunkId chunkId) tag -> wraparg2 "with-id" (show wrapKind) chunkId $ show tag
        WithClass wrapKind (ChunkClass chunkClass) tag -> wraparg2 "with-class" (show wrapKind) chunkClass $ show tag
        Custom name args tag -> wraparg "custom" name $ show tag
        where
            just title = "(" <> title <> ")"
            wrap title v = "(" <> title <> ":" <> v <> ")"
            wraparg title arg v = "(" <> title <> "(" <> arg <> "):" <> v <> ")"
            wraparg2 title arg1 arg2 v = "(" <> title <> "(" <> arg1 <> "," <> arg2 <> "):" <> v <> ")"
            wraptag2 title tag1 tag2 = "(" <> title <> ":" <> tag1 <> "," <> tag2 <> ")"
            wraplist title vals = "(" <> title <> ":" <> String.joinWith "," vals <> ")"
            wraplistarg title arg vals = "(" <> title <> "(" <> arg <> "):" <> String.joinWith "," vals <> ")"
            wraplistarg2 title arg1 arg2 vals = "(" <> title <> "(" <> arg1 <> "," <> arg2 <> "):" <> String.joinWith "," vals <> ")"
            -- tableitems t ts = show t <> ":" <> String.joinWith "," (show <$> ts)
-}

instance Show Align where
    show Left = "left"
    show Right = "right"
    show Center = "center"


instance Show Bullet where
    show None = "none"
    show Disc = "disc"
    show Asterisk = "asterisk"
    show Dash = "dash"
    show Circle = "circle"
    show Alpha = "alpha"
    show AlphaInv = "alphainv"
    show Num = "num"
    show (BCustom str) = "custom:" <> str


instance Show HLevel where
    show = case _ of
        H1 -> "H1"
        H2 -> "H2"
        H3 -> "H3"
        H4 -> "H4"
        H5 -> "H5"
        H6 -> "H6"


instance Show Format where
    show = case _ of
        Bold -> "bold"
        Emphasis -> "emphasis"
        Underline -> "underline"
        Highlight -> "highlight"
        Blink -> "blink"
        Inverse -> "inverse"
        Invisible -> "invisible"
        Strikethrough -> "striked"
        Monospaced -> "mono"
        FixedWidth -> "fixed"
        Quote mbAuthor -> "quote" <> case mbAuthor of
            Just (QuoteOf author) -> "(" <> author <> ")"
            Nothing -> ""
        Sub -> "sub"
        Sup -> "sup"
        Verbatim -> "verbatim"
        Footnote ftnId -> "footnote#" <> show ftnId
        Code lang -> "code:" <> show lang
        Header level anchor -> "header#" <> show level <> case anchor of
                                            Just anchor -> "{" <> show anchor <> "}"
                                            Nothing -> ""
        Link (Url url) -> "link:" <> show url
        InlineImage _ (Url url) -> "inline-image:" <> show url
        LinkTo (FootnoteId ftnId) -> case ftnId of
            E.Left intId -> "footnote:#" <> show intId
            E.Right strId -> "footnote:" <> strId
        Comment -> "comment"
        Fg ecolor ->
            "fg(" <> case ecolor of
                    E.Left colorStr -> show colorStr
                    E.Right color -> show color <> ")"
        Bg ecolor ->
            "bg(" <> case ecolor of
                    E.Left colorStr -> show colorStr
                    E.Right color -> show color <> ")"

{-
instance Show TermAndDefinition where
    show (TAndD (Term term /\ Definition def)) = "(" <> show term <> " :: " <> show def <> ")"
-}


instance Show ImageSide where
    show = case _ of
        Auto -> "auto"
        Px n -> show n <> "px"


instance Show ImageParams where
    show (ImageParams rec) = "{" <> show rec.width <> "x" <> show rec.height <> "," <> show rec.caption <> "}"