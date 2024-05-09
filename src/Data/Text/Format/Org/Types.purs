module Data.Text.Format.Org.Types where

import Prelude

import Foreign (Foreign, F)
import Prim.RowList as RL

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String (CodePoint)
import Data.Map (Map)
import Data.Map as Map
import Data.Date (Day, Weekday)
import Data.Tuple as Tuple
import Data.Time (Time(..))
import Data.Time as Time
import Data.Enum (fromEnum, toEnum)
-- import Data.Time (TimeOfDay)
import Data.Variant (Variant)
import Data.Variant (match) as Variant
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodePoints (codePointFromChar)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Bifunctor (lmap)

import Yoga.JSON (class ReadForeign, class WriteForeign, class ReadForeignFields, class WriteForeignFields, class ReadForeignVariant, class WriteForeignVariant, readImpl, writeImpl)
import Yoga.Json.Extra (Case, Case1, Case2, readMatchImpl)
import Yoga.Json.Extra
    ( mark, matched, match1, match2, select1, select2, todo
    , uncase, uncase1, uncase2 ) as Variant

-- inspired by https://hackage.haskell.org/package/org-mode-2.1.0/docs/Data-Org.html


data OrgFile =
    OrgFile
        { meta :: Map (Int /\ String) String
        , doc :: OrgDoc
        }


newtype OrgDoc =
    OrgDoc
        { zeroth :: Array Block
        , sections :: Array Section
        }


data Block
    = Greater GreaterBlockKind String
    | List ListItems
    | Table (NonEmptyArray TableRow)
    | Paragraph (NonEmptyArray Words)
    -- | JoinB Block Block


data Words
    = Marked MarkupKey String
    | Link LinkTarget (Maybe String)
    | Image ImageSource
    | Punct CodePoint
    | Plain String
    | Markup String
    | Break
    -- | Space
    -- | Indent Int
    | JoinW Words Words


data GreaterBlockKind 
    = Quote
    | Example
    | Code (Maybe Language) 
    | Custom String (Array String)


data MarkupKey 
    = Bold 
    | Italic
    | Highlight 
    | Underline
    | Verbatim
    | InlineCode
    | Strike
    | Error
    | And MarkupKey MarkupKey


newtype OrgDateTime =
    OrgDateTime
        { day :: Day
        , dayOfWeek :: Weekday
        , time :: Maybe OrgTimeRange
        , repeat :: Maybe Repeater
        , delay :: Maybe Delay
        }


newtype OrgTimeRange =
    OrgTimeRange
        { start :: Time
        , end :: Maybe Time
        }


newtype Repeater =
    Repeater
        { mode :: RepeaterMode
        , value :: Int
        , interval :: Interval
        }


data RepeaterMode
    = Single
    | Jump
    | FromToday


data Interval = Hour | Day | Week | Month | Year


newtype Delay =
    Delay
        { mode :: DelayMode
        , value :: Int
        , interval :: Interval
        }


data DelayMode
    = DelayOne
    | DelayAll


newtype Drawer =
    Drawer
        { name :: String
        , content :: NonEmptyArray Words
        }


newtype Planning =
    Planning
        { closed :: Maybe OrgDateTime
        , deadline :: Maybe OrgDateTime
        , scheduled :: Maybe OrgDateTime
        , timestamp :: Maybe OrgDateTime
        }


newtype Section =
    Section
        { todo :: Maybe Todo
        , priority :: Maybe Priority
        , cookie :: Maybe Cookie
        , check :: Maybe Check -- TODO:  shouldn't be in section?
        , heading :: NonEmptyArray Words
        , level :: Int
        , tags :: Array String
        , planning :: Planning
        , props :: Map String String
        , drawers :: Array Drawer
        , comment :: Boolean
        , doc :: OrgDoc
        }


data Todo
    = Todo
    | Doing
    | Done
    | CustomKW String


data Priority
    = Alpha Char
    | Num Int


data Cookie
    = Split
    | Percent
    | Pie


data Check
    = Check
    | Uncheck
    | Halfcheck


data Counter 
    = Counter Int    


data ListType
    = Bulleted
    | Plussed
    | Numbered
    | NumberedFrom Int
    | Hyphened
    | Alphed


data TableRow = BreakT | Row (NonEmptyArray TableColumn)


data TableColumn = Empty | Column (NonEmptyArray Words)


data ListItems = ListItems ListType (NonEmptyArray Item)


data Item = 
    Item 
        { check :: Maybe Check
        , counter :: Maybe Counter 
        , tag :: Maybe String
        } 
        (NonEmptyArray Words) 
        (Maybe ListItems)


data LinkTarget 
    = Remote String
    | Local String
    | Heading String


data ImageSource 
    = RemoteSrc String
    | LocalSrc String


newtype Language = Language String


{- ----- Newtype ------- -}

derive instance Newtype Language _
derive instance Newtype Drawer _
derive instance Newtype OrgDateTime _
derive instance Newtype OrgTimeRange _
derive instance Newtype Repeater _
derive instance Newtype Delay _
derive instance Newtype Planning _

-- internals below, they don't need Newtype instance

-- derive instance Newtype Section _
-- derive instance Newtype OrgDoc _
-- derive instance Newtype OrgFile _

{- ----- Show & Eq ----- -}


instance Show Check where
    show = case _ of
        Check -> "check"
        Uncheck -> "un-check"
        Halfcheck -> "half-check"

derive instance Eq Check


{- ----- JSON ----- -}


class JsonOverRow (row :: Row Type) a | a -> row where
    convert :: a -> Record row
    load :: Record row -> a


class JsonOverRow row a <= JsonOverRowH (row :: Row Type) a | a -> row where
    readImplRow :: Foreign -> F a
    writeImplRow :: a -> Foreign


class JsonOverVariant (row :: Row Type) a | a -> row where
    readForeign :: Foreign -> F a
    toVariant :: a -> Variant row
    fromVariant :: Variant row -> a -- TODO: could be used / joined with `readForeign`, or `readForeign` could use `fromVariant` as binding


class JsonOverVariant row a <= JsonOverVariantH (row :: Row Type) a | a -> row where
    readImplVar :: Foreign -> F a
    writeImplVar :: a -> Foreign


class Newtype a x <= JsonOverNewtype a x | a -> x, x -> a where
    readImplNT :: Foreign -> F a
    writeImplNT :: a -> Foreign


instance (RL.RowToList row rl, ReadForeignVariant rl row, WriteForeignVariant rl row, JsonOverVariant row a) => JsonOverVariantH row a where
    readImplVar = readForeign
    writeImplVar = writeImpl <<< toVariant


instance (ReadForeign x, WriteForeign x, Newtype a x) => JsonOverNewtype a x where
    readImplNT f = (readImpl f :: F x) <#> wrap
    writeImplNT = unwrap >>> writeImpl


instance
    ( RL.RowToList row rl
    , ReadForeignFields rl () row
    , WriteForeignFields rl row () to
    , JsonOverRow row a
    ) => JsonOverRowH row a where
    readImplRow f = (readImpl f :: F (Record row)) <#> load
    writeImplRow = convert >>> writeImpl


-- instance (ReadForeign x, JsonOverNewtype a x) => ReadForeign a where
--     readImpl f = (readImpl f :: F x) <#> wrap


type CheckRow =
    ( check :: Case
    , uncheck :: Case
    , halfcheck :: Case
    )


readCheck :: Foreign -> F Check
readCheck =
    readMatchImpl
        (Proxy :: _ CheckRow)
        { check : Variant.matched Check
        , uncheck : Variant.matched Uncheck
        , halfcheck : Variant.matched Halfcheck
        }


checkToVariant :: Check -> Variant CheckRow
checkToVariant = case _ of
    Check -> Variant.mark (Proxy :: _ "check")
    Uncheck -> Variant.mark (Proxy :: _ "uncheck")
    Halfcheck -> Variant.mark (Proxy :: _ "halfcheck")



checkFromVariant :: Variant CheckRow -> Check
checkFromVariant =
    Variant.match
        { check : Variant.uncase Check
        , uncheck : Variant.uncase Uncheck
        , halfcheck : Variant.uncase Halfcheck
        }


instance ReadForeign Check where readImpl = readImplVar
instance WriteForeign Check where writeImpl = writeImplVar
instance JsonOverVariant CheckRow Check where
    readForeign = readCheck
    toVariant = checkToVariant
    fromVariant = checkFromVariant


instance ReadForeign Language where readImpl = readImplNT
instance WriteForeign Language where writeImpl = writeImplNT


type LinkTargetRow =
    ( remote :: Case1 String
    , local :: Case1 String
    , heading :: Case1 String
    )


readLinkTarget :: Foreign -> F LinkTarget
readLinkTarget =
    readMatchImpl
        (Proxy :: _ LinkTargetRow)
        { remote : Variant.match1 Remote
        , local : Variant.match1 Local
        , heading : Variant.match1 Heading
        }


linkTargetToVariant :: LinkTarget -> Variant LinkTargetRow
linkTargetToVariant = case _ of
    Remote url  -> Variant.select1 (Proxy :: _ "remote") url
    Local url   -> Variant.select1 (Proxy :: _ "local") url
    Heading trg -> Variant.select1 (Proxy :: _ "heading") trg



linkTargetFromVariant :: Variant LinkTargetRow -> LinkTarget
linkTargetFromVariant =
    Variant.match
        { remote : Variant.uncase1 >>> Remote
        , local : Variant.uncase1 >>> Local
        , heading : Variant.uncase1 >>> Heading
        }


instance ReadForeign LinkTarget where readImpl = readImplVar
instance WriteForeign LinkTarget where writeImpl = writeImplVar
instance JsonOverVariant LinkTargetRow LinkTarget where
    readForeign = readLinkTarget
    toVariant = linkTargetToVariant
    fromVariant = linkTargetFromVariant


type ImageSourceRow =
    ( remote :: Case1 String
    , local :: Case1 String
    )


readImageSource :: Foreign -> F ImageSource
readImageSource =
    readMatchImpl
        (Proxy :: _ ImageSourceRow)
        { remote : Variant.match1 RemoteSrc
        , local : Variant.match1 LocalSrc
        }


imageSourceToVariant :: ImageSource -> Variant ImageSourceRow
imageSourceToVariant = case _ of
    RemoteSrc url -> Variant.select1 (Proxy :: _ "remote") url
    LocalSrc url  -> Variant.select1 (Proxy :: _ "local") url


imageSourceFromVariant :: Variant ImageSourceRow -> ImageSource
imageSourceFromVariant =
    Variant.match
        { remote : Variant.uncase1 >>> RemoteSrc
        , local : Variant.uncase1 >>> LocalSrc
        }


instance ReadForeign ImageSource where readImpl = readImplVar
instance WriteForeign ImageSource where writeImpl = writeImplVar
instance JsonOverVariant ImageSourceRow ImageSource where
    readForeign = readImageSource
    toVariant = imageSourceToVariant
    fromVariant = imageSourceFromVariant    


type GreaterBlockKindRow =
    ( quote :: Case
    , example :: Case
    , code :: Case1 (Maybe Language)
    , custom :: Case2 String (Array String)
    )


readGreaterBlockKind :: Foreign -> F GreaterBlockKind
readGreaterBlockKind =
    readMatchImpl
        (Proxy :: _ GreaterBlockKindRow)
        { quote : Variant.matched Quote
        , example : Variant.matched Example
        , code : Variant.match1 Code
        , custom : Variant.match2 Custom
        }    


greaterBlockKindToVariant :: GreaterBlockKind -> Variant GreaterBlockKindRow
greaterBlockKindToVariant = case _ of 
    Quote -> Variant.mark (Proxy :: _ "quote")
    Example -> Variant.mark (Proxy :: _ "example")
    Code mbLang -> Variant.select1 (Proxy :: _ "code") mbLang
    Custom name args -> Variant.select2 (Proxy :: _ "custom") name args


greaterBlockKindFromVariant :: Variant GreaterBlockKindRow -> GreaterBlockKind
greaterBlockKindFromVariant =
    Variant.match
        { quote : Variant.uncase Quote
        , example : Variant.uncase Example
        , code : Variant.uncase1 >>> Code
        , custom : Variant.uncase2 >>> Tuple.uncurry Custom
        }


instance ReadForeign GreaterBlockKind where readImpl = readImplVar
instance WriteForeign GreaterBlockKind where writeImpl = writeImplVar
instance JsonOverVariant GreaterBlockKindRow GreaterBlockKind where
    readForeign = readGreaterBlockKind
    toVariant = greaterBlockKindToVariant
    fromVariant = greaterBlockKindFromVariant        


type BlockRow =
    ( greater :: Case2 GreaterBlockKind String
    -- , list :: ListItems
    -- , table :: Array TableRow
    , paragraph :: Case1 (Array Words)
    )


toNEA :: forall a. a -> Array a -> NonEmptyArray a
toNEA a = NEA.fromArray >>> fromMaybe (NEA.singleton a)


readBlock :: Foreign -> F Block
readBlock =
    readMatchImpl
        (Proxy :: _ BlockRow)
        { greater : Variant.match2 Greater
        -- , list : ?wh -- Variant.todo $ Quote ""
        -- , table : ?wh -- Variant.todo $ Quote ""
        , paragraph : Variant.match1 $ Paragraph <<< toNEA (Plain "??") -- FIXME
        }


blockToVariant :: Block -> Variant BlockRow
blockToVariant = case _ of
    Greater kind value -> Variant.select2 (Proxy :: _ "greater") kind value
    Paragraph words -> Variant.select1 (Proxy :: _ "paragraph") $ NEA.toArray words
    List _ -> Variant.select2 (Proxy :: _ "greater") Quote "QQQ" -- FIXME
    Table _ -> Variant.select2 (Proxy :: _ "greater") Quote "QQQ" -- FIXME


blockFromVariant :: Variant BlockRow -> Block
blockFromVariant =
    Variant.match
        { greater : Variant.uncase2 >>> Tuple.uncurry Greater
        -- , list : ?wh -- Variant.todo $ Quote ""
        -- , table : ?wh -- Variant.todo $ Quote ""
        , paragraph : Variant.uncase1 >>> toNEA (Plain "??") >>> Paragraph
        }


instance ReadForeign Block where readImpl = readImplVar
instance WriteForeign Block where writeImpl = writeImplVar
instance JsonOverVariant BlockRow Block where
    readForeign = readBlock
    toVariant = blockToVariant
    fromVariant = blockFromVariant


type MarkupKeyRow =
    ( bold :: Case
    , italic :: Case
    , highlight :: Case
    , underline :: Case
    , verbatim :: Case
    , inlineCode :: Case
    , strike :: Case
    , error :: Case
    )    


readMarkupKey :: Foreign -> F MarkupKey
readMarkupKey =
    readMatchImpl
        (Proxy :: _ MarkupKeyRow)
        { bold : Variant.matched Bold
        , italic : Variant.matched Italic
        , highlight : Variant.matched Highlight
        , underline : Variant.matched Underline
        , verbatim : Variant.matched Verbatim
        , inlineCode : Variant.matched InlineCode
        , strike : Variant.matched Strike
        , error : Variant.matched Error -- FIXME
        }    


markupKeyToVariant :: MarkupKey -> Variant MarkupKeyRow
markupKeyToVariant = case _ of
    Bold -> Variant.mark (Proxy :: _ "bold")
    Italic -> Variant.mark (Proxy :: _ "italic")
    Highlight -> Variant.mark (Proxy :: _ "highlight")
    Underline -> Variant.mark (Proxy :: _ "underline")
    Verbatim -> Variant.mark (Proxy :: _ "verbatim")
    InlineCode -> Variant.mark (Proxy :: _ "inlineCode")
    Strike -> Variant.mark (Proxy :: _ "strike")
    Error -> Variant.mark (Proxy :: _ "error")
    And _ _ -> Variant.mark (Proxy :: _ "error") -- we do not encode `And`, we unwrap it in a list of other keys


markupKeyFromVariant :: Variant MarkupKeyRow -> MarkupKey
markupKeyFromVariant =
    Variant.match
        { bold : Variant.uncase Bold
        , italic : Variant.uncase Italic
        , highlight : Variant.uncase Highlight
        , underline : Variant.uncase Underline
        , verbatim : Variant.uncase Verbatim
        , inlineCode : Variant.uncase InlineCode
        , strike : Variant.uncase Strike
        , error : Variant.uncase Error -- FIXME
        }


markupKeyToRowArray :: MarkupKey -> Array (Variant MarkupKeyRow)
markupKeyToRowArray = case _ of 
    And keyA keyB -> markupKeyToRowArray keyA <> markupKeyToRowArray keyB
    key -> Array.singleton $ markupKeyToVariant key


rowArrayToMarkupKey :: Array (Variant MarkupKeyRow) -> MarkupKey
rowArrayToMarkupKey vars =
    case NEA.fromArray vars of 
        Just nea -> nea <#> fromVariant # NEA.foldl1 And
        Nothing -> Error



instance ReadForeign MarkupKey where readImpl = readImplVar
instance WriteForeign MarkupKey where writeImpl = writeImplVar
instance JsonOverVariant MarkupKeyRow MarkupKey where
    readForeign = readMarkupKey
    toVariant = markupKeyToVariant
    fromVariant = markupKeyFromVariant        


type WordsRow =
    ( link :: Case2 LinkTarget (Maybe String)
    , image :: Case1 ImageSource
    , punct :: Case1 Char
    , plain :: Case1 String
    , markup :: Case1 String
    , break :: Case
    , marked :: Case2 (Array (Variant MarkupKeyRow)) String
    -- , join :: Words /\ Words
    )


readWords :: Foreign -> F Words
readWords =
    readMatchImpl
        (Proxy :: _ WordsRow)
        { marked : Variant.match2 $ Marked <<< rowArrayToMarkupKey
        , link : Variant.match2 Link
        , image : Variant.match1 Image
        , punct : Variant.match1 $ Punct <<< codePointFromChar
        , plain : Variant.match1 Plain
        , markup : Variant.match1 Markup
        , break : Variant.matched Break
        -- , join : Variant.match2 JoinW
        }


wordsToVariant :: Words -> Variant WordsRow
wordsToVariant = case _ of
    Marked key s -> Variant.select2 (Proxy :: _ "marked") (markupKeyToRowArray key) s
    Link url mbStr -> Variant.select2 (Proxy :: _ "link") url mbStr
    Image url -> Variant.select1 (Proxy :: _ "image") url
    Punct _ -> Variant.select1 (Proxy :: _ "punct") $ ':' -- FIXME
    Plain p -> Variant.select1 (Proxy :: _ "plain") p
    Markup mup -> Variant.select1 (Proxy :: _ "markup") mup
    Break -> Variant.mark (Proxy :: _ "break")
    JoinW wA wB -> Variant.select1 (Proxy :: _ "plain") "JOIN" -- FIXME


wordsFromVariant :: Variant WordsRow -> Words
wordsFromVariant =
    Variant.match
        { marked : Variant.uncase2 >>> lmap rowArrayToMarkupKey >>> Tuple.uncurry Marked
        , link : Variant.uncase2 >>> Tuple.uncurry Link
        , image : Variant.uncase1 >>> Image
        , punct : Variant.uncase1 >>> codePointFromChar >>> Punct
        , plain : Variant.uncase1 >>> Plain
        , markup : Variant.uncase1 >>> Markup
        , break : Variant.uncase Break
        -- , join : Variant.uncase2 JoinW  -- FIXME
        }


instance ReadForeign Words where readImpl = readImplVar
instance WriteForeign Words where writeImpl = writeImplVar
instance JsonOverVariant WordsRow Words where
    readForeign = readWords
    toVariant = wordsToVariant
    fromVariant = wordsFromVariant


type CookieRow =
    ( split :: Case
    , percent :: Case
    , pie :: Case
    )


readCookie :: Foreign -> F Cookie
readCookie =
    readMatchImpl
        (Proxy :: _ CookieRow)
        { split : Variant.matched Split
        , percent : Variant.matched Percent
        , pie : Variant.matched Pie
        }


cookieToVariant :: Cookie -> Variant CookieRow
cookieToVariant = case _ of
    Split -> Variant.mark (Proxy :: _ "split")
    Percent -> Variant.mark (Proxy :: _ "percent")
    Pie -> Variant.mark (Proxy :: _ "pie")


cookieFromVariant :: Variant CookieRow -> Cookie
cookieFromVariant =
    Variant.match
        { split : Variant.uncase Split
        , percent : Variant.uncase Percent
        , pie : Variant.uncase Pie
        }


instance ReadForeign Cookie where readImpl = readImplVar
instance WriteForeign Cookie where writeImpl = writeImplVar
instance JsonOverVariant CookieRow Cookie where
    readForeign = readCookie
    toVariant = cookieToVariant
    fromVariant = cookieFromVariant


type PriorityRow =
    ( alpha :: Case1 Char
    , num :: Case1 Int
    )


readPriority :: Foreign -> F Priority
readPriority =
    readMatchImpl
        (Proxy :: _ PriorityRow)
        { alpha : Variant.match1 Alpha
        , num : Variant.match1 Num
        }


priorityToVariant :: Priority -> Variant PriorityRow
priorityToVariant = case _ of
    Alpha a -> Variant.select1 (Proxy :: _ "alpha") a
    Num n -> Variant.select1 (Proxy :: _ "num") n


priorityFromVariant :: Variant PriorityRow -> Priority
priorityFromVariant =
    Variant.match
        { alpha : Variant.uncase1 >>> Alpha
        , num : Variant.uncase1 >>> Num
        }


instance ReadForeign Priority where readImpl = readImplVar
instance WriteForeign Priority where writeImpl = writeImplVar
instance JsonOverVariant PriorityRow Priority where
    readForeign = readPriority
    toVariant = priorityToVariant
    fromVariant = priorityFromVariant


type TodoRow =
    ( todo :: Case
    , doing :: Case
    , done :: Case
    , custom :: Case1 String
    )


readTodo :: Foreign -> F Todo
readTodo =
    readMatchImpl
        (Proxy :: _ TodoRow)
        { todo : Variant.matched Todo
        , doing : Variant.matched Doing
        , done : Variant.matched Done
        , custom : Variant.match1 CustomKW
        }


todoToVariant :: Todo -> Variant TodoRow
todoToVariant = case _ of
    Todo -> Variant.mark (Proxy :: _ "todo")
    Doing -> Variant.mark (Proxy :: _ "doing")
    Done -> Variant.mark (Proxy :: _ "done")
    CustomKW s -> Variant.select1 (Proxy :: _ "custom") s


todoFromVariant :: Variant TodoRow -> Todo
todoFromVariant =
    Variant.match
        { todo : Variant.uncase Todo
        , doing : Variant.uncase Doing
        , done : Variant.uncase Done
        , custom : Variant.uncase1 >>> CustomKW
        }


instance ReadForeign Todo where readImpl = readImplVar
instance WriteForeign Todo where writeImpl = writeImplVar
instance JsonOverVariant TodoRow Todo where
    readForeign = readTodo
    toVariant = todoToVariant
    fromVariant = todoFromVariant


type ListTypeRow =
    ( bulleted :: Case
    , plussed :: Case
    , numbered :: Case
    , numberedFrom :: Case1 Int
    , hyphened :: Case
    , alphed :: Case
    )


readListType :: Foreign -> F ListType
readListType =
    readMatchImpl
        (Proxy :: _ ListTypeRow)
        { bulleted : Variant.matched Bulleted
        , plussed : Variant.matched Plussed
        , numbered : Variant.matched Numbered
        , numberedFrom : Variant.match1 NumberedFrom
        , hyphened : Variant.matched Hyphened
        , alphed : Variant.matched Alphed
        }


listTypeToVariant :: ListType -> Variant ListTypeRow
listTypeToVariant = case _ of
    Bulleted -> Variant.mark (Proxy :: _ "bulleted")
    Plussed -> Variant.mark (Proxy :: _ "plussed")
    Numbered -> Variant.mark (Proxy :: _ "numbered")
    NumberedFrom n -> Variant.select1 (Proxy :: _ "numberedFrom") n
    Hyphened -> Variant.mark (Proxy :: _ "hyphened")
    Alphed -> Variant.mark (Proxy :: _ "alphed")


listTypeFromVariant :: Variant ListTypeRow -> ListType
listTypeFromVariant =
    Variant.match
        { bulleted : Variant.uncase Bulleted
        , plussed : Variant.uncase Plussed
        , numbered : Variant.uncase Numbered
        , numberedFrom : Variant.uncase1 >>> NumberedFrom
        , hyphened : Variant.uncase Hyphened
        , alphed : Variant.uncase Alphed
        }


instance ReadForeign ListType where readImpl = readImplVar
instance WriteForeign ListType where writeImpl = writeImplVar
instance JsonOverVariant ListTypeRow ListType where
    readForeign = readListType
    toVariant = listTypeToVariant
    fromVariant = listTypeFromVariant


type IntervalRow =
    ( hour :: Case
    , day :: Case
    , week :: Case
    , month :: Case
    , year :: Case
    )


readInterval :: Foreign -> F Interval
readInterval =
    readMatchImpl
        (Proxy :: _ IntervalRow)
        { hour : Variant.matched Hour
        , day : Variant.matched Day
        , week : Variant.matched Week
        , month : Variant.matched Month
        , year : Variant.matched Year
        }


intervalToVariant :: Interval -> Variant IntervalRow
intervalToVariant = case _ of
    Hour  -> Variant.mark (Proxy :: _ "hour")
    Day   -> Variant.mark (Proxy :: _ "day")
    Week  -> Variant.mark (Proxy :: _ "week")
    Month -> Variant.mark (Proxy :: _ "month")
    Year  -> Variant.mark (Proxy :: _ "year")


intervalFromVariant :: Variant IntervalRow -> Interval
intervalFromVariant =
    Variant.match
        { hour : Variant.uncase Hour
        , day : Variant.uncase Day
        , week : Variant.uncase Week
        , month : Variant.uncase Month
        , year : Variant.uncase Year
        }


instance ReadForeign Interval where readImpl = readImplVar
instance WriteForeign Interval where writeImpl = writeImplVar
instance JsonOverVariant IntervalRow Interval where
    readForeign = readInterval
    toVariant = intervalToVariant
    fromVariant = intervalFromVariant


type RepeaterModeRow =
    ( single :: Case
    , jump :: Case
    , fromToday :: Case
    )


readRepeaterMode :: Foreign -> F RepeaterMode
readRepeaterMode =
    readMatchImpl
        (Proxy :: _ RepeaterModeRow)
        { single : Variant.matched Single
        , jump : Variant.matched Jump
        , fromToday : Variant.matched FromToday
        }


repeaterModeToVariant :: RepeaterMode -> Variant RepeaterModeRow
repeaterModeToVariant = case _ of
    Single    -> Variant.mark (Proxy :: _ "single")
    Jump      -> Variant.mark (Proxy :: _ "jump")
    FromToday -> Variant.mark (Proxy :: _ "fromToday")


repeaterModeFromVariant :: Variant RepeaterModeRow -> RepeaterMode
repeaterModeFromVariant =
    Variant.match
        { single : Variant.uncase Single
        , jump : Variant.uncase Jump
        , fromToday : Variant.uncase FromToday
        }


instance ReadForeign RepeaterMode where readImpl = readImplVar
instance WriteForeign RepeaterMode where writeImpl = writeImplVar
instance JsonOverVariant RepeaterModeRow RepeaterMode where
    readForeign = readRepeaterMode
    toVariant = repeaterModeToVariant
    fromVariant = repeaterModeFromVariant


type DelayModeRow =
    ( one :: Case
    , all :: Case
    )


readDelayMode :: Foreign -> F DelayMode
readDelayMode =
    readMatchImpl
        (Proxy :: _ DelayModeRow)
        { one : Variant.matched DelayOne
        , all : Variant.matched DelayAll
        }


delayModeToVariant :: DelayMode -> Variant DelayModeRow
delayModeToVariant = case _ of
    DelayOne -> Variant.mark (Proxy :: _ "one")
    DelayAll -> Variant.mark (Proxy :: _ "all")


delayModeFromVariant :: Variant DelayModeRow -> DelayMode
delayModeFromVariant mode =
    flip Variant.match mode $
        { one : const DelayOne
        , all : const DelayAll
        }


instance ReadForeign DelayMode where readImpl = readImplVar
instance WriteForeign DelayMode where writeImpl = writeImplVar
instance JsonOverVariant DelayModeRow DelayMode where
    readForeign = readDelayMode
    toVariant = delayModeToVariant
    fromVariant = delayModeFromVariant


instance ReadForeign Drawer where readImpl = readImplNT
instance WriteForeign Drawer where writeImpl = writeImplNT


-- instance ReadForeign OrgDateTime where readImpl f = readImpl f <#> wrap
-- instance WriteForeign OrgDateTime where writeImpl = unwrap >>> writeImpl


-- instance ReadForeign OrgTime where readImpl f = readImpl f <#> wrap
-- instance WriteForeign OrgTime where writeImpl = unwrap >>> writeImpl


instance ReadForeign Repeater where readImpl = readImplNT
instance WriteForeign Repeater where writeImpl = writeImplNT


-- instance ReadForeign OrgDoc where readImpl f = readImpl f <#> wrap
-- instance WriteForeign OrgDoc where writeImpl = unwrap >>> writeImpl


-- instance ReadForeign Section where readImpl f = readImpl f <#> wrap
-- instance WriteForeign Section where writeImpl = unwrap >>> writeImpl


type DelayRow =
    ( mode :: Variant DelayModeRow
    , value :: Int
    , interval :: Interval
    )


convertDelay :: Delay -> Record DelayRow
convertDelay = unwrap >>>
    case _ of
        { mode, value, interval } ->
            { mode : toVariant mode
            , value
            , interval
            }


loadDelay :: Record DelayRow -> Delay
loadDelay =
    case _ of
        { mode, value, interval } ->
            wrap
                { mode : fromVariant mode
                , value
                , interval
                }


instance ReadForeign Delay where readImpl = readImplNT
instance WriteForeign Delay where writeImpl = writeImplNT
instance JsonOverRow DelayRow Delay where
    convert = convertDelay
    load = loadDelay


type JsonTimeRow =
    ( hour :: Int
    , minute :: Int
    , second :: Int
    , millisecond :: Int
    -- TODO: zone :: String
    )


convertTime :: Time -> Record JsonTimeRow
convertTime t =
    { hour : Time.hour t # fromEnum
    , minute : Time.minute t # fromEnum
    , second : Time.second t # fromEnum
    , millisecond : Time.millisecond t # fromEnum
    }


loadTime :: Record JsonTimeRow -> Time
loadTime rec =
    Time
        (toEnum rec.hour # fromMaybe bottom)
        (toEnum rec.minute # fromMaybe bottom)
        (toEnum rec.second # fromMaybe bottom)
        (toEnum rec.millisecond # fromMaybe bottom)


instance JsonOverRow JsonTimeRow Time where
    convert = convertTime
    load = loadTime


type JsonTimeRangeRow =
    ( start :: Record JsonTimeRow
    , end :: Maybe (Record JsonTimeRow)
    )


convertTimeRange :: OrgTimeRange -> Record JsonTimeRangeRow
convertTimeRange t =
    { start : convert  $ _.start $ unwrap t
    , end :   convert <$> (_.end $ unwrap t)
    }


loadTimeRange :: Record JsonTimeRangeRow -> OrgTimeRange
loadTimeRange rec =
    OrgTimeRange
        { start : load rec.start
        , end : load <$> rec.end
        }


instance JsonOverRow JsonTimeRangeRow OrgTimeRange where
    convert = convertTimeRange
    load = loadTimeRange


type JsonDateTimeRow =
    ( day :: Int
    , dayOfWeek :: Int
    , time :: Maybe (Record JsonTimeRangeRow)
    , repeat :: Maybe Repeater
    , delay :: Maybe Delay
    )


convertToDateTime :: OrgDateTime -> Record JsonDateTimeRow
convertToDateTime = unwrap >>> case _ of
    { day, dayOfWeek, time, repeat, delay } ->
        { day : fromEnum day
        , dayOfWeek : fromEnum dayOfWeek
        , time : convert <$> time
        , delay
        , repeat
        }


loadDateTime :: Record JsonDateTimeRow -> OrgDateTime
loadDateTime =
    case _ of
        { day, dayOfWeek, time, repeat, delay } ->
            wrap
                { day : toEnum day # fromMaybe bottom
                , dayOfWeek : toEnum dayOfWeek # fromMaybe bottom
                , time : load <$> time
                , delay
                , repeat
                }


instance JsonOverRow JsonDateTimeRow OrgDateTime where
    convert = convertToDateTime
    load = loadDateTime


convertDateTimeNT :: OrgDateTime -> JsonDateTime
convertDateTimeNT = convert >>> wrap


loadDateTimeNT :: JsonDateTime -> OrgDateTime
loadDateTimeNT = unwrap >>> load


newtype JsonDateTime = JsonDateTime (Record JsonDateTimeRow)


derive instance Newtype JsonDateTime _


instance ReadForeign JsonDateTime where readImpl = readImplNT
instance WriteForeign JsonDateTime where writeImpl = writeImplNT


newtype JsonSectionId = SectionId (Array Int)


derive instance Newtype JsonSectionId _


derive newtype instance Eq JsonSectionId
derive newtype instance Ord JsonSectionId


instance ReadForeign JsonSectionId where readImpl = readImplNT
instance WriteForeign JsonSectionId where writeImpl = writeImplNT


type PlanningRow =
    ( closed :: Maybe JsonDateTime
    , deadline :: Maybe JsonDateTime
    , scheduled :: Maybe JsonDateTime
    , timestamp :: Maybe JsonDateTime
    )


convertPlanning :: Planning -> Record PlanningRow
convertPlanning = unwrap >>> case _ of
    pl ->
        { closed    : convertDateTimeNT <$> pl.closed
        , deadline  : convertDateTimeNT <$> pl.deadline
        , scheduled : convertDateTimeNT <$> pl.scheduled
        , timestamp : convertDateTimeNT <$> pl.timestamp
        }


loadPlanning :: Record PlanningRow -> Planning
loadPlanning pl =
    wrap
        { closed : loadDateTimeNT <$> pl.closed
        , deadline : loadDateTimeNT <$> pl.deadline
        , scheduled : loadDateTimeNT <$> pl.scheduled
        , timestamp : loadDateTimeNT <$> pl.timestamp
        }


instance JsonOverRow PlanningRow Planning where
    convert = convertPlanning
    load = loadPlanning


-- convertDoc :: OrgDoc -> Record DocRow
-- convertOrgFile :: OrgFile -> Record FileRow


type DocRow =
    ( blocks :: Array (Variant BlockRow)
    , sections :: Array JsonSectionId
    )


type SectionRow =
    ( id :: JsonSectionId
     -- FIXME: We may use original types instead of `Variant` here and below since `readForeign`/`writeForeign` are implemented for them
    , todo :: Maybe (Variant TodoRow)
    , priority :: Maybe (Variant PriorityRow)
    , cookie :: Maybe (Variant CookieRow)
    , check :: Maybe (Variant CheckRow)
    , heading :: Array Words
    , level :: Int
    , planning :: Record PlanningRow
    , props :: Map String String
    , comment :: Boolean
    -- , drawers :: Array Drawer -- FIXME: TODO
    , doc :: Record DocRow
    )


type FileRow =
    ( meta :: Array (Int /\ String /\ String)
    , doc :: Record DocRow
    , sections :: Array (JsonSectionId /\ Record SectionRow)
    )


type SectionsMap = Map JsonSectionId (Record SectionRow)


sectionsToArray :: SectionsMap -> Array (JsonSectionId /\ Record SectionRow)
sectionsToArray = Map.toUnfoldable


sectionsFromArray :: Array (JsonSectionId /\ Record SectionRow) -> SectionsMap
sectionsFromArray = Map.fromFoldable


emptyPlanning :: Planning
emptyPlanning =
    Planning
        { closed : Nothing
        , deadline : Nothing
        , scheduled : Nothing
        , timestamp : Nothing
        }


emptyDoc :: OrgDoc
emptyDoc =
    OrgDoc
        { zeroth : []
        , sections : []
        }


emptySection :: Section
emptySection =
    Section
        { todo: Nothing
        , priority : Nothing
        , cookie : Nothing
        , check : Nothing
        , heading : NEA.singleton $ Plain "$$" -- FIXME: TODO
        , level : -1
        , tags : []
        , planning : emptyPlanning
        , props : Map.empty
        , drawers : []
        , comment : false
        , doc : emptyDoc
        }


convertSection :: JsonSectionId -> Section -> Record SectionRow /\ SectionsMap
convertSection sectionId (Section section) =
    let convertedDoc /\ sectionsMap = convertDoc sectionId section.doc
    in
        { id : sectionId
        , todo : toVariant <$> section.todo
        , priority : toVariant <$> section.priority
        , cookie : toVariant <$> section.cookie
        , check : toVariant <$> section.check
        , heading : [] -- FIXME: TODO
        , level : section.level
        , planning : convert section.planning
        , props : Map.empty -- FIXME: TODO
        , comment : section.comment
        , doc : convertedDoc
        }
    /\ sectionsMap


loadSection :: SectionsMap -> Record SectionRow -> Section
loadSection allSections section =
    Section
        { todo : fromVariant <$> section.todo
        , priority : fromVariant <$> section.priority
        , cookie : fromVariant <$> section.cookie
        , check : fromVariant <$> section.check
        , heading : NEA.singleton $ Plain "$$" -- FIXME: TODO
        , level : section.level
        , tags : [] -- FIXME: TODO
        , planning : load section.planning
        , drawers : [] -- FIXME: TODO
        , props : Map.empty  -- FIXME: TODO
        , comment : section.comment
        , doc : loadDoc allSections section.doc
        }


-- instance ReadForeign Section where
--     readImpl f = (readImpl f :: F (Record SectionRow)) <#> loadSection (SectionsMap Map.empty)


-- instance JsonOverRow DocRow OrgDoc where
--     convert = convertSection
--     load = loadSection


collectSections :: JsonSectionId -> Array Section -> Array JsonSectionId /\ SectionsMap
collectSections (SectionId parentId) sections =
    let
        sectionsIdsAndInnerMaps =
            sections
                # Array.mapWithIndex
                    (\idx section ->
                        let
                            sectionId = SectionId $ parentId <> [ idx ]
                            convertedSection /\ innerSections = convertSection sectionId section
                        in
                            sectionId /\ (innerSections # Map.insert sectionId convertedSection)
                    )
        sectionsIds = Tuple.fst <$> sectionsIdsAndInnerMaps
        sectionsMap = Array.foldl Map.union Map.empty $ Tuple.snd <$> sectionsIdsAndInnerMaps
    in sectionsIds /\ sectionsMap


convertDoc :: JsonSectionId -> OrgDoc -> Record DocRow /\ SectionsMap
convertDoc parentId (OrgDoc doc) =
    let (sectionsIds /\ sectionsMap) = collectSections parentId doc.sections
    in
    (
        { blocks : toVariant <$> doc.zeroth
        , sections : sectionsIds
        }
    /\
        sectionsMap
    )


-- instance ReadForeign OrgDoc where
--     readImpl f = (readImpl f :: F (Record DocRow)) <#> loadDoc Map.empty


loadDoc :: SectionsMap -> Record DocRow -> OrgDoc
loadDoc allSections doc =
    OrgDoc
        { zeroth : fromVariant <$> doc.blocks
        , sections : loadOrEmpty <$> doc.sections
        }
    where
        loadOrEmpty sectionId =
            Map.lookup sectionId allSections
                <#> loadSection allSections
                 #  fromMaybe emptySection


-- instance JsonOverRow DocRow OrgDoc where
--     convert = convertDoc
--     load = loadDoc


convertFile :: OrgFile -> Record FileRow
convertFile (OrgFile { meta, doc }) =
    let convertedDoc /\ sectionsMap = convertDoc (SectionId [ 0 ]) doc
    in
    { meta : _fromOrderedMap meta
    , doc : convertedDoc
    , sections : sectionsToArray sectionsMap
    }


loadFile :: Record FileRow -> OrgFile
loadFile file =
    OrgFile
        { meta : _toOrderedMap file.meta
        , doc : loadDoc (sectionsFromArray file.sections) file.doc
        }


_toOrderedMap :: Array (Int /\ String /\ String) -> Map (Int /\ String) String
_toOrderedMap = Map.fromFoldable <<< map _swap3


_fromOrderedMap :: Map (Int /\ String) String -> Array (Int /\ String /\ String)
_fromOrderedMap = map _bswap3 <<< Map.toUnfoldable


-- t a (t b c) ->  t (t a b) c
_swap3 :: forall a b c. a /\ (b /\ c) -> (a /\ b) /\ c
_swap3 (a /\ (b /\ c)) = (a /\ b) /\ c


-- t (t a b) c ->  t a (t b c)
_bswap3 :: forall a b c. (a /\ b) /\ c -> a /\ (b /\ c)
_bswap3 ((a /\ b) /\ c) = a /\ (b /\ c)


-- convertFileNT :: OrgFile -
-- convertFileNT = convert >>> wrap


-- loadFileNT :: Record FileRow -> OrgFile
-- loadFileNT = unwrap >>> load


instance JsonOverRow FileRow OrgFile where
    convert = convertFile
    load = loadFile


-- instance Newtype (Array Int) JsonSectionId


instance ReadForeign OrgFile where readImpl = readImplRow
instance WriteForeign OrgFile where writeImpl = writeImplRow