module Data.Text.Format.Org.Types where

import Prelude

import Foreign (Foreign, F)
import Prim.RowList as RL

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe, fromMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String (CodePoint)
import Data.Map (Map)
import Data.Date (Day, Weekday)
import Data.Time (Time(..))
import Data.Time as Time
import Data.Enum (fromEnum, toEnum)
-- import Data.Time (TimeOfDay)
import Data.Variant (Variant)
import Data.Variant (match) as Variant
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodePoints (codePointFromChar)
import Data.Newtype (class Newtype, wrap, unwrap)

import Yoga.JSON (class ReadForeign, class WriteForeign, class ReadForeignFields, class WriteForeignFields, class ReadForeignVariant, class WriteForeignVariant, readImpl, writeImpl)
import Yoga.Json.Extra (Case, Case1, Case2, readMatchImpl)
import Yoga.Json.Extra (mark, matched, match1, match2, select1, select2, todo) as Variant

-- inspired by https://hackage.haskell.org/package/org-mode-2.1.0/docs/Data-Org.html


data OrgFile =
    OrgFile
        { meta :: Map String String
        , doc :: OrgDoc
        }


newtype OrgDoc =
    OrgDoc
        { zeroth :: Array Block
        , sections :: Array Section
        }


data Block
    = Quote String
    | Example String
    | Code (Maybe Language) String
    | List ListItems
    | Table (NonEmptyArray TableRow)
    | Paragraph (NonEmptyArray Words)
    -- | JoinB Block Block


data Words
    = Bold String
    | Italic String
    | Highlight String
    | Underline String
    | Verbatim String
    | Strike String
    | Link URL (Maybe String)
    | Image URL
    | Punct CodePoint
    | Plain String
    | Markup String
    -- | BreakW
    -- | Space
    -- | Indent Int
    | JoinW Words Words


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
        , check :: Maybe Check
        , heading :: NonEmptyArray Words
        , level :: Int
        , tags :: Array String
        , planning :: Planning
        , props :: Map String String
        , drawers :: Array Drawer
        , doc :: OrgDoc
        }


data Todo
    = TODO
    | DOING
    | DONE
    | Custom String


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


data ListType
    = Bulleted
    | Plussed
    | Numbered
    | Alphed


data TableRow = Break | Row (NonEmptyArray TableColumn)


data TableColumn = Empty | Column (NonEmptyArray Words)


data ListItems = ListItems ListType (NonEmptyArray Item)


data Item = Item (NonEmptyArray Words) (Maybe ListItems)


newtype URL = URL String


newtype Language = Language String


{- ----- Newtype ------- -}

derive instance Newtype URL _
derive instance Newtype Language _
derive instance Newtype Section _
derive instance Newtype OrgDoc _
derive instance Newtype Drawer _
derive instance Newtype OrgDateTime _
derive instance Newtype OrgTimeRange _
derive instance Newtype Repeater _
derive instance Newtype Delay _
derive instance Newtype Planning _

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


class JsonOverRow row a <= JsonOverRowH (row :: Row Type) a | row -> a where
    readImplRow :: Foreign -> F (Record row)
    writeImplRow :: (Record row) -> Foreign


class JsonOverVariant (row :: Row Type) a | a -> row where
    readForeign :: Foreign -> F a
    toVariant :: a -> Variant row


class JsonOverVariant row a <= JsonOverVariantH (row :: Row Type) a | a -> row where
    readImplVar :: Foreign -> F a
    writeImplVar :: a -> Foreign


class Newtype a x <= JsonOverNewtype a x | a -> x where
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
    -- FIXME: we don't need that class / instance since it's just `readImpl` / `writeImpl`
    readImplRow = readImpl
    writeImplRow = writeImpl


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


instance ReadForeign Check where readImpl = readImplVar
instance WriteForeign Check where writeImpl = writeImplVar
instance JsonOverVariant CheckRow Check where
    readForeign = readCheck
    toVariant = checkToVariant


instance ReadForeign Language where readImpl = readImplNT
instance WriteForeign Language where writeImpl = writeImplNT


instance ReadForeign URL where readImpl = readImplNT
instance WriteForeign URL where writeImpl = writeImplNT


type BlockRow =
    ( quote :: Case1 String
    , example :: Case1 String
    , code :: Case2 (Maybe Language) String
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
        { quote : Variant.match1 Quote
        , example : Variant.match1 Example
        , code : Variant.match2 Code
        -- , list : ?wh -- Variant.todo $ Quote ""
        -- , table : ?wh -- Variant.todo $ Quote ""
        , paragraph : Variant.match1 $ Paragraph <<< toNEA (Plain "??")
        }


blockToVariant :: Block -> Variant BlockRow
blockToVariant = case _ of
    Quote q ->  Variant.select1 (Proxy :: _ "quote") q
    Example ex ->  Variant.select1 (Proxy :: _ "example") ex
    Code mbLang value -> Variant.select2 (Proxy :: _ "code") mbLang value
    Paragraph words -> Variant.select1 (Proxy :: _ "paragraph") $ NEA.toArray words
    List _ -> Variant.select1 (Proxy :: _ "quote") "QQQ" -- FIXME
    Table _ -> Variant.select1 (Proxy :: _ "quote") "QQQ" -- FIXME


instance ReadForeign Block where readImpl = readImplVar
instance WriteForeign Block where writeImpl = writeImplVar
instance JsonOverVariant BlockRow Block where
    readForeign = readBlock
    toVariant = blockToVariant


type WordsRow =
    ( bold :: Case1 String
    , italic :: Case1 String
    , highlight :: Case1 String
    , underline :: Case1 String
    , verbatim :: Case1 String
    , strike :: Case1 String
    , link :: Case2 URL (Maybe String)
    , image :: Case1 URL
    , punct :: Case1 Char
    , plain :: Case1 String
    , markup :: Case1 String
    -- , join :: Words /\ Words
    )


readWords :: Foreign -> F Words
readWords =
    readMatchImpl
        (Proxy :: _ WordsRow)
        { bold : Variant.match1 Bold
        , italic : Variant.match1 Italic
        , highlight : Variant.match1 Highlight
        , underline : Variant.match1 Underline
        , verbatim : Variant.match1 Verbatim
        , link : Variant.match2 Link
        , image : Variant.match1 Image
        , punct : Variant.match1 $ Punct <<< codePointFromChar
        , strike : Variant.match1 Strike
        , plain : Variant.match1 Plain
        , markup : Variant.match1 Markup
        -- , join : Variant.match2 JoinW
        }


wordsToVariant :: Words -> Variant WordsRow
wordsToVariant = case _ of
    Bold b -> Variant.select1 (Proxy :: _ "bold") b
    Italic i -> Variant.select1 (Proxy :: _ "italic") i
    Highlight hl -> Variant.select1 (Proxy :: _ "highlight") hl
    Underline ul -> Variant.select1 (Proxy :: _ "underline") ul
    Verbatim v -> Variant.select1 (Proxy :: _ "verbatim") v
    Link url mbStr -> Variant.select2 (Proxy :: _ "link") url mbStr
    Image url -> Variant.select1 (Proxy :: _ "image") url
    Punct _ -> Variant.select1 (Proxy :: _ "punct") $ ':' -- FIXME
    Strike s -> Variant.select1 (Proxy :: _ "strike") s
    Plain p -> Variant.select1 (Proxy :: _ "plain") p
    Markup mup -> Variant.select1 (Proxy :: _ "markup") mup
    JoinW wA wB -> Variant.select1 (Proxy :: _ "bold") "JOIN" -- FIXME


instance ReadForeign Words where readImpl = readImplVar
instance WriteForeign Words where writeImpl = writeImplVar
instance JsonOverVariant WordsRow Words where
    readForeign = readWords
    toVariant = wordsToVariant


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


instance ReadForeign Cookie where readImpl = readImplVar
instance WriteForeign Cookie where writeImpl = writeImplVar
instance JsonOverVariant CookieRow Cookie where
    readForeign = readCookie
    toVariant = cookieToVariant


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


instance ReadForeign Priority where readImpl = readImplVar
instance WriteForeign Priority where writeImpl = writeImplVar
instance JsonOverVariant PriorityRow Priority where
    readForeign = readPriority
    toVariant = priorityToVariant


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
        { todo : Variant.matched TODO
        , doing : Variant.matched DOING
        , done : Variant.matched DONE
        , custom : Variant.match1 Custom
        }


todoToVariant :: Todo -> Variant TodoRow
todoToVariant = case _ of
    TODO -> Variant.mark (Proxy :: _ "todo")
    DOING -> Variant.mark (Proxy :: _ "doing")
    DONE -> Variant.mark (Proxy :: _ "done")
    Custom s -> Variant.select1 (Proxy :: _ "custom") s


instance ReadForeign Todo where readImpl = readImplVar
instance WriteForeign Todo where writeImpl = writeImplVar
instance JsonOverVariant TodoRow Todo where
    readForeign = readTodo
    toVariant = todoToVariant


type ListTypeRow =
    ( bulleted :: Case
    , plussed :: Case
    , numbered :: Case
    , alphed :: Case
    )


readListType :: Foreign -> F ListType
readListType =
    readMatchImpl
        (Proxy :: _ ListTypeRow)
        { bulleted : Variant.matched Bulleted
        , plussed : Variant.matched Plussed
        , numbered : Variant.matched Numbered
        , alphed : Variant.matched Alphed
        }


listTypeToVariant :: ListType -> Variant ListTypeRow
listTypeToVariant = case _ of
    Bulleted -> Variant.mark (Proxy :: _ "bulleted")
    Plussed -> Variant.mark (Proxy :: _ "plussed")
    Numbered -> Variant.mark (Proxy :: _ "numbered")
    Alphed -> Variant.mark (Proxy :: _ "alphed")


instance ReadForeign ListType where readImpl = readImplVar
instance WriteForeign ListType where writeImpl = writeImplVar
instance JsonOverVariant ListTypeRow ListType where
    readForeign = readListType
    toVariant = listTypeToVariant


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


instance ReadForeign Interval where readImpl = readImplVar
instance WriteForeign Interval where writeImpl = writeImplVar
instance JsonOverVariant IntervalRow Interval where
    readForeign = readInterval
    toVariant = intervalToVariant


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


instance ReadForeign RepeaterMode where readImpl = readImplVar
instance WriteForeign RepeaterMode where writeImpl = writeImplVar
instance JsonOverVariant RepeaterModeRow RepeaterMode where
    readForeign = readRepeaterMode
    toVariant = repeaterModeToVariant


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


convertDelayMode :: Variant DelayModeRow -> DelayMode
convertDelayMode mode =
    flip Variant.match mode $
        { one : const DelayOne
        , all : const DelayAll
        }


instance ReadForeign DelayMode where readImpl = readImplVar
instance WriteForeign DelayMode where writeImpl = writeImplVar
instance JsonOverVariant DelayModeRow DelayMode where
    readForeign = readDelayMode
    toVariant = delayModeToVariant


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
            { mode : delayModeToVariant mode
            , value
            , interval
            }


loadDelay :: Record DelayRow -> Delay
loadDelay =
    case _ of
        { mode, value, interval } ->
            wrap
                { mode : convertDelayMode mode
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
    { start : convertTime  $ _.start $ unwrap t
    , end :   convertTime <$> (_.end $ unwrap t)
    }


loadTimeRange :: Record JsonTimeRangeRow -> OrgTimeRange
loadTimeRange rec =
    OrgTimeRange
        { start : loadTime rec.start
        , end : loadTime <$> rec.end
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
        , time : convertTimeRange <$> time
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
                , time : loadTimeRange <$> time
                , delay
                , repeat
                }


instance JsonOverRow JsonDateTimeRow OrgDateTime where
    convert = convertToDateTime
    load = loadDateTime


convertDateTimeNT :: OrgDateTime -> JsonDateTime
convertDateTimeNT = convertToDateTime >>> wrap


loadDateTimeNT :: JsonDateTime -> OrgDateTime
loadDateTimeNT = unwrap >>> loadDateTime


newtype JsonDateTime = JsonDateTime (Record JsonDateTimeRow)


derive instance Newtype JsonDateTime _


instance ReadForeign JsonDateTime where readImpl = readImplNT
instance WriteForeign JsonDateTime where writeImpl = writeImplNT


newtype JsonSectionId = SectionId (Array Int)


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
    ( blocks :: Array Block
    , sections :: Array JsonSectionId
    )


type SectionRow =
    ( todo :: Maybe Todo
    , priority :: Maybe Priority
    , cookie :: Maybe Cookie
    , heading :: Array Words
    , level :: Int
    , planning :: Record PlanningRow
    , props :: Map String String
    , drawers :: Array Drawer
    , doc :: Record DocRow
    )


type FileRow =
    ( meta :: Map String String
    , doc :: Record DocRow
    , sections :: Map JsonSectionId (Record SectionRow)
    )