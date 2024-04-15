module Data.Text.Format.Org.Types where

import Prelude
import Prim hiding (Row)

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.String (CodePoint)
import Data.Map (Map)
import Data.Date (Day, Weekday)
-- import Data.Time (TimeOfDay)


data OrgFile =
    OrgFile
        { meta :: Map String String
        , doc :: OrgDoc
        }


data OrgDoc =
    OrgDoc
        { blocks :: Array Block
        , sections :: Array Section
        }


data Block
    = Quote String
    | Example String
    | Code (Maybe Language) String
    | List ListItems
    | Table (NonEmptyArray Row)
    | Paragraph (NonEmptyArray Words)


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


data OrgDateTime =
    OrgDateTime
        { day :: Day
        , dayOfWeek :: Weekday
        , time :: Maybe OrgTime
        , repeat :: Maybe Repeater
        , delay :: Maybe Delay
        }


data TimeOfDay


data OrgTime =
    OrgTime
        { start :: TimeOfDay
        , end :: Maybe TimeOfDay
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


data Check
    = Check
    | Uncheck
    | Halfcheck


data ListType
    = Bulleted
    | Plussed
    | Numbered
    | Alphed


data Row = Break | Row (NonEmptyArray Column)


data Column = Empty | Column (NonEmptyArray Words)


data ListItems = ListItems ListType (NonEmptyArray Item)


data Item = Item (NonEmptyArray Words) (Maybe ListItems)


newtype URL = URL String


newtype Language = Language String
