module Data.Text.Format.Org.Types where

import Prelude

import Foreign (Foreign, F)

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe, fromMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String (CodePoint)
import Data.Map (Map)
import Data.Date (Day, Weekday)
import Data.Time (Time)
-- import Data.Time (TimeOfDay)
import Data.Variant (Variant)
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodePoints (codePointFromChar)
import Data.Newtype (class Newtype, wrap, unwrap)

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
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
        , time :: Maybe OrgTime
        , repeat :: Maybe Repeater
        , delay :: Maybe Delay
        }


newtype OrgTime =
    OrgTime
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


newtype Section =
    Section
        { todo :: Maybe Todo
        , priority :: Maybe Priority
        , cookie :: Maybe Cookie
        , check :: Maybe Check
        , heading :: NonEmptyArray Words
        , level :: Int
        , tags :: Array String
        , planning ::
            { closed :: Maybe OrgDateTime
            , deadline :: Maybe OrgDateTime
            , scheduled :: Maybe OrgDateTime
            , timestamp :: Maybe OrgDateTime
            }
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
derive instance Newtype OrgTime _
derive instance Newtype Repeater _
derive instance Newtype Delay _

{- ----- Show & Eq ----- -}


instance Show Check where
    show = case _ of
        Check -> "check"
        Uncheck -> "un-check"
        Halfcheck -> "half-check"

derive instance Eq Check


{- ----- JSON ----- -}


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


instance ReadForeign Check where readImpl = readCheck
instance WriteForeign Check where writeImpl = writeImpl <<< checkToVariant


instance ReadForeign Language where readImpl f = (readImpl f :: F String) <#> wrap
instance WriteForeign Language where writeImpl = unwrap >>> writeImpl


instance ReadForeign URL where readImpl f = (readImpl f :: F String) <#> wrap
instance WriteForeign URL where writeImpl = unwrap >>> writeImpl


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


instance ReadForeign Words where readImpl = readWords
instance WriteForeign Words where writeImpl = writeImpl <<< wordsToVariant


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


instance ReadForeign Cookie where readImpl = readCookie
instance WriteForeign Cookie where writeImpl = writeImpl <<< cookieToVariant


type PriotityRow =
    ( alpha :: Case1 Char
    , num :: Case1 Int
    )


readPriority :: Foreign -> F Priority
readPriority =
    readMatchImpl
        (Proxy :: _ PriotityRow)
        { alpha : Variant.match1 Alpha
        , num : Variant.match1 Num
        }


priorityToVariant :: Priority -> Variant PriotityRow
priorityToVariant = case _ of
    Alpha a -> Variant.select1 (Proxy :: _ "alpha") a
    Num n -> Variant.select1 (Proxy :: _ "num") n


instance ReadForeign Priority where readImpl = readPriority
instance WriteForeign Priority where writeImpl = writeImpl <<< priorityToVariant


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


instance ReadForeign Todo where readImpl = readTodo
instance WriteForeign Todo where writeImpl = writeImpl <<< todoToVariant


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


instance ReadForeign ListType where readImpl = readListType
instance WriteForeign ListType where writeImpl = writeImpl <<< listTypeToVariant


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


instance ReadForeign Interval where readImpl = readInterval
instance WriteForeign Interval where writeImpl = writeImpl <<< intervalToVariant


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


instance ReadForeign RepeaterMode where readImpl = readRepeaterMode
instance WriteForeign RepeaterMode where writeImpl = writeImpl <<< repeaterModeToVariant


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


instance ReadForeign DelayMode where readImpl = readDelayMode
instance WriteForeign DelayMode where writeImpl = writeImpl <<< delayModeToVariant


instance ReadForeign Drawer where readImpl f = readImpl f <#> wrap
instance WriteForeign Drawer where writeImpl = unwrap >>> writeImpl


-- instance ReadForeign OrgDateTime where readImpl f = readImpl f <#> wrap
-- instance WriteForeign OrgDateTime where writeImpl = unwrap >>> writeImpl


-- instance ReadForeign OrgTime where readImpl f = readImpl f <#> wrap
-- instance WriteForeign OrgTime where writeImpl = unwrap >>> writeImpl


instance ReadForeign Repeater where readImpl f = readImpl f <#> wrap
instance WriteForeign Repeater where writeImpl = unwrap >>> writeImpl


-- instance ReadForeign OrgDoc where readImpl f = readImpl f <#> wrap
-- instance WriteForeign OrgDoc where writeImpl = unwrap >>> writeImpl


-- instance ReadForeign Section where readImpl f = readImpl f <#> wrap
-- instance WriteForeign Section where writeImpl = unwrap >>> writeImpl