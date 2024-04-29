module Yoga.Json.Extra where

import Prelude

import Prim.Row as Row
import Prim.RowList (class RowToList, RowList) as RL

import Type.Proxy (Proxy)

import Data.Either (Either(..), hush)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, class VariantMatchCases)
import Data.Variant (match, inj) as Variant

import Control.Monad.Except (except)

import Foreign (F, Foreign, fail, ForeignError(..))

import Yoga.JSON (class ReadForeign, class WriteForeign, class ReadForeignVariant, readImpl, writeImpl)


data Case = Case -- a.k.a. Unit


instance ReadForeign Case where
    readImpl f = (readImpl f :: F String) >>= (\str -> case str of
        "." -> except $ Right Case
        _ -> fail $ ForeignError "No match")


instance WriteForeign Case where
    writeImpl = const $ writeImpl "."


readMatchImpl
    :: forall
        (row :: Row Type)
        (rec :: Row Type)
        (a :: Type)
        (rl :: RL.RowList Type)
        (rl1 :: RL.RowList Type)
        (rl2 :: Row Type)
     . ReadForeignVariant rl row
    => RL.RowToList row rl
    => RL.RowToList rec rl1
    => VariantMatchCases rl1 rl2 (F a)
    => Row.Union rl2 () row
    => Proxy row -> Record rec -> Foreign -> F a
readMatchImpl _ rec f =
    (readImpl f :: F (Variant row))
        >>= Variant.match rec :: F a


mark
    :: forall (label :: Symbol) (row' :: Row Type) (row ::Row Type)
     . Row.Cons label Case row' row ⇒ IsSymbol label
    => Proxy label → Variant row
mark = flip Variant.inj Case


matched :: forall a. a -> (Case -> F a)
matched = const <<< except <<< Right