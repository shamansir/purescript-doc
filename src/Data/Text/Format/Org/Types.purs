module Data.Text.Format.Org.Types where

import Prelude
-- import Prim hiding (Row)
-- import Prim (Row) as Prim
import Prim.Row as R

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.String (CodePoint)
import Data.Map (Map)
import Data.Date (Day, Weekday)
import Data.Time (Time)
-- import Data.Time (TimeOfDay)
import Data.Variant (Variant, class VariantMatchCases)
import Data.Variant (match) as Variant
import Control.Monad.Except (except)

import Yoga.JSON (class ReadForeign, class ReadForeignVariant, readImpl, readVariantImpl)
import Yoga.Json.Extra (NoParams(..), readMatchImpl)
import Foreign (Foreign, F)
import Type.Proxy (Proxy(..))
import Prim.RowList (class RowToList, Cons, Nil, RowList)

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


{- ----- JSON ----- -}


type CheckRow =
    ( "check" :: NoParams
    , "uncheck" :: NoParams
    , "halfcheck" :: NoParams
    )

instance ReadForeign Check where
    readImpl = readCheck


{-
readCheck :: Foreign -> F Check
readCheck f =
    (readImpl f :: F (Variant CheckRow))
        >>= Variant.match
            { check : \_ -> except $ Right Check
            , uncheck : \_ -> except $ Right Uncheck
            , halfcheck : \_ -> except $ Right Halfcheck
            } :: F Check
-}



readCheck :: Foreign -> F Check
readCheck =
    readMatchImpl
        (Proxy :: _ CheckRow)
        { check : \_ -> except $ Right Check
        , uncheck : \_ -> except $ Right Uncheck
        , halfcheck : \_ -> except $ Right Halfcheck
        }


-- instance WriteForeign Check where
--     writeImpl check = writeImpl (checkToVariant check)