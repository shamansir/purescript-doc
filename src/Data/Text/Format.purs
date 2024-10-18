module Data.Text.Format where

import Prelude

import Color (Color)

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Either (Either(..)) as E
import Data.String (joinWith) as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype)


newtype Indent = Indent Int
newtype Level = Level Int
newtype FootnoteId = FootnoteId (Either Int String)
newtype Anchor = Anchor String
newtype ProgrammingLanguage = ProgrammingLanguage String
newtype Url = Url String
newtype Term = Term Tag
newtype Definition = Definition Tag
newtype TermAndDefinition = TAndD (Term /\ Definition)
newtype ImageParams =
        ImageParams
            { width :: ImageSide
            , height :: ImageSide
            }


derive instance Newtype Indent _
derive instance Newtype Level _
derive instance Newtype FootnoteId _
derive instance Newtype Anchor _
derive instance Newtype ProgrammingLanguage _
derive instance Newtype Url _
derive instance Newtype Term _
derive instance Newtype Definition _
derive instance Newtype TermAndDefinition _
derive instance Newtype ImageParams _


derive newtype instance Show Indent
derive newtype instance Show Level
derive newtype instance Show FootnoteId
derive newtype instance Show Anchor
derive newtype instance Show ProgrammingLanguage
derive newtype instance Show Term
derive newtype instance Show Definition



data Align
    = Left
    | Right
    | Center


data Format
    = Bold
    | Emphasis
    | Underline
    | Highlight
    | Blink
    | Inverse
    | Invisible
    | Strikethrough
    | Monospaced
    | Header Level (Maybe Anchor)
    | Quote
    | Verbatim
    | Link Url
    | Image ImageParams Url
    | Footnote FootnoteId
    | LinkTo FootnoteId
    | Code ProgrammingLanguage
    | Define Term
    | Comment
    | FixedWidth
    | Sub
    | Sup
    | Fg (Either String Color)
    | Bg (Either String Color)


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
    -- ..


data Tag
    = Empty
    | Plain String
    | Format Format Tag
    | Align Align Tag
    | Split Tag Tag
    | Pair Tag Tag
    | Join Tag (Array Tag)
    | Wrap Tag Tag Tag
    | Para (Array Tag)
    | Nest Indent (Array Tag)
    | Newline
    | List Bullet Tag (Array Tag) -- The root `Tag`` is optional (if `Empty`) header of the list
    | DefList (Array TermAndDefinition)
    -- | Table (Array (Tag /\ Array Tag))
    | Table (Array Tag) (Array (Array Tag))
    | Hr

-- TODO: binary operators for tags
-- TODO: empty tag

data ImageSide -- FIXME: reuse come other type
    = Auto
    | Px Int


instance Semigroup Tag where
    append :: Tag -> Tag -> Tag
    append = Pair


s :: String -> Tag
s = plain


plain :: String -> Tag
plain = Plain


lefts :: String -> Tag
lefts = left <<< plain


left :: Tag -> Tag
left = Align Left


rights :: String -> Tag
rights = right <<< plain


right :: Tag -> Tag
right = Align Right


centers :: String -> Tag
centers = center <<< plain


center :: Tag -> Tag
center = Align Center


bolds :: String -> Tag
bolds = bold <<< plain


bold :: Tag -> Tag
bold = Format Bold


em :: Tag -> Tag
em = Format Emphasis


i :: Tag -> Tag
i = Format Emphasis


u :: Tag -> Tag
u = Format Underline


thru :: Tag -> Tag
thru = Format Strikethrough


mono :: Tag -> Tag
mono = Format Monospaced


underlines :: String -> Tag
underlines = underline <<< plain


underline :: Tag -> Tag
underline = Format Underline


blinks :: String -> Tag
blinks = blink <<< plain


blink :: Tag -> Tag
blink = Format Blink


inverses :: String -> Tag
inverses = inverse <<< plain


inverse :: Tag -> Tag
inverse = Format Inverse


invisibles :: String -> Tag
invisibles = invisible <<< plain


invisible :: Tag -> Tag
invisible = Format Invisible


wrap :: Tag -> Tag -> Tag -> Tag
wrap = Wrap


wraps :: String -> String -> Tag -> Tag
wraps l r = Wrap (Plain l) (Plain r)


url :: String -> Url
url = Url


link :: Url -> Tag -> Tag
link = Format <<< Link


img :: Url -> Tag -> Tag
img = imgp $ ImageParams { width : Auto, height : Auto }


imgp :: ImageParams -> Url -> Tag -> Tag
imgp par = Format <<< Image par


sized :: { w :: Int, h :: Int } -> ImageParams
sized { w, h } = ImageParams { width : Px w, height : Px h }


ftn :: String -> Tag -> Tag
ftn = Format <<< Footnote <<< FootnoteId <<< E.Right


ftni :: Int -> Tag -> Tag
ftni = Format <<< Footnote <<< FootnoteId <<< E.Left


to_ftn :: String -> Tag -> Tag
to_ftn = Format <<< LinkTo <<< FootnoteId <<< E.Right


to_ftni :: Int -> Tag -> Tag
to_ftni = Format <<< LinkTo <<< FootnoteId <<< E.Left



h :: Int -> Tag -> Tag
h lvl = Format $ Header (Level lvl) Nothing


h' :: Int -> String -> Tag -> Tag
h' lvl = Format <<< Header (Level lvl) <<< Just <<< Anchor


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


define :: String -> Tag -> Tag
define = Format <<< Define <<< Term <<< Plain


dt :: Tag -> Tag -> TermAndDefinition
dt term def = TAndD $ Term term /\ Definition def


dl :: Array TermAndDefinition -> Tag
dl = DefList


f :: Format -> Tag -> Tag
f = Format


none = None :: Bullet
disk = Disc :: Bullet
asterisk = Asterisk :: Bullet
dash = Dash :: Bullet
num = Num :: Bullet
alpha = Alpha :: Bullet
circle = Circle :: Bullet
nalpha = AlphaInv :: Bullet


list :: Tag -> Array Tag -> Tag
list = List Dash


list_ :: Array Tag -> Tag
list_ = List Dash Empty



listb :: Bullet -> Tag -> Array Tag -> Tag
listb = List


listb_ :: Bullet -> Array Tag -> Tag
listb_ bul = List bul Empty


stack :: Array Tag -> Tag
stack = Para -- nest 0?


fgcs :: Color -> String -> Tag
fgcs c = fgc c <<< plain


fgc :: Color -> Tag -> Tag
fgc = Format <<< Fg <<< E.Right


fgs :: String -> String -> Tag
fgs cs = fg cs <<< plain


fg :: String -> Tag -> Tag
fg = Format <<< Fg <<< E.Left


bgcs :: Color -> String -> Tag
bgcs c = bgc c <<< plain


bgc :: Color -> Tag -> Tag
bgc = Format <<< Bg <<< E.Right


bgs :: String -> String -> Tag
bgs cs = bg cs <<< plain


bg :: String -> Tag -> Tag
bg = Format <<< Bg <<< E.Left


split :: Tag -> Tag -> Tag
split = Split


nl :: Tag
nl = Newline


nest :: Int -> Array Tag -> Tag
nest n = Nest $ Indent n


indent :: Int -> Tag -> Tag
indent n = nest n <<< singleton


group :: Array Tag -> Tag
group = nest 0


joinWith :: Tag -> Array Tag -> Tag
joinWith = Join


code :: String -> String -> Tag
code pl = Format (Code $ ProgrammingLanguage pl) <<< Plain


tableh :: Array Tag -> Array (Array Tag) -> Tag
tableh = Table


table :: Array (Array Tag) -> Tag
table = tableh []


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
    Alpha -> "a." -- FIXME: TODO
    AlphaInv -> "z." -- FIXME: TODO
    Num -> "1" -- FIXME: TODO


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



-- TODO: do syntax?

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
            Image imgParams (Url url) -> let title = tag in wraparg2 "image" (show title) (show imgParams) $ show url
            Comment -> wrap "comment" $ show tag
            Footnote (FootnoteId ftnId) ->
                case ftnId of
                    E.Left ftnIntId -> wraparg "footnote" ("#" <> show ftnIntId) $ show tag
                    E.Right ftnStrId -> wraparg "footnote" ftnStrId $ show tag
            _ -> wraparg "format" (show format) $ show tag
        Split tag1 tag2 -> wraptag2 "split" (show tag1) $ show tag2
        Pair tag1 tag2 -> wraptag2 "pair" (show tag1) $ show tag2
        Para tags -> wraplist "para" $ show <$> tags
        Newline -> just "nl"
        Hr -> just "hr"
        Nest indent tags -> wraplistarg "nest" (show indent) $ show <$> tags
        Join tag tags -> wraplistarg "join" (show tag) $ show <$> tags
        Wrap start end tag -> wraparg2 "wrap" (show start) (show end) $ show tag
        List bullet tag tags -> wraplistarg2 "list" (show bullet) (show tag) $ show <$> tags
        DefList definitions -> wraplist "list" $ show <$> definitions
        Table headers rows -> wrap "table" $ (wrap "header" $ String.joinWith "," $ show <$> headers) <> "|" <> (wraplist "row" $ wraplist "column" <$> map show <$> rows)
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
        Quote -> "quote"
        Sub -> "sub"
        Sup -> "sup"
        Verbatim -> "verbatim"
        Footnote ftnId -> "footnote#" <> show ftnId
        Code lang -> "code:" <> show lang
        Header level anchor -> "header#" <> show level <> case anchor of
                                            Just anchor -> "{" <> show anchor <> "}"
                                            Nothing -> ""
        Link (Url url) -> "link:" <> show url
        Image _ (Url url) -> "image:" <> show url
        LinkTo (FootnoteId ftnId) -> case ftnId of
            E.Left intId -> "footnote:#" <> show intId
            E.Right strId -> "footnote:" <> strId
        Define (Term term) -> "define:" <> show term
        Comment -> "comment"
        Fg ecolor ->
            "fg(" <> case ecolor of
                    E.Left colorStr -> show colorStr
                    E.Right color -> show color <> ")"
        Bg ecolor ->
            "bg(" <> case ecolor of
                    E.Left colorStr -> show colorStr
                    E.Right color -> show color <> ")"


instance Show TermAndDefinition where
    show (TAndD (Term term /\ Definition def)) = "(" <> show term <> " :: " <> show def <> ")"


instance Show ImageParams where
    show (ImageParams rec) = "{}"