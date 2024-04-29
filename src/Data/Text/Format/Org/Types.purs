module Data.Text.Format.Org.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.String (CodePoint)
import Data.Map (Map)
import Data.Date (Day, Weekday)
import Data.Time (Time)
-- import Data.Time (TimeOfDay)
import Data.Variant (Variant)

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Yoga.Json.Extra (Case, readMatchImpl)
import Yoga.Json.Extra (mark, matched) as Variant
import Foreign (Foreign, F)
import Type.Proxy (Proxy(..))

-- inspired by https://hackage.haskell.org/package/org-mode-2.1.0/docs/Data-Org.html


data OrgFile =
    OrgFile
        { meta :: Map String String
        , doc :: OrgDoc
        }


data OrgDoc =
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
    | JoinB Block Block


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
    -- | Break
    -- | Space
    -- | Indent Int
    | JoinW Words Words


data OrgDateTime =
    OrgDateTime
        { day :: Day
        , dayOfWeek :: Weekday
        , time :: Maybe OrgTime
        , repeat :: Maybe Repeater
        , delay :: Maybe Delay
        }


data OrgTime =
    OrgTime
        { start :: Time
        , end :: Maybe Time
        }


data Repeater =
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


data Delay =
    Delay
        { mode :: DelayMode
        , value :: Int
        , interval :: Interval
        }


data DelayMode
    = DelayOne
    | DelayAll


data Drawer =
    Drawer
        { name :: String
        , content :: NonEmptyArray Words
        }


data Section =
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


{- ----- Show & Eq ----- -}


instance Show Check where
    show = case _ of
        Check -> "check"
        Uncheck -> "un-check"
        Halfcheck -> "half-check"

derive instance Eq Check


{- ----- JSON ----- -}


type CheckRow =
    ( "check" :: Case
    , "uncheck" :: Case
    , "halfcheck" :: Case
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