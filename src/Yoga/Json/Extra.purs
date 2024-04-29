module Yoga.Json.Extra where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, throwError, withExcept)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, on, class VariantMatchCases)
import Data.Variant (match) as Variant
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, isNull, isUndefined, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, tagOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record (get)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (Proxy(..))

import Yoga.JSON (class ReadForeign, readImpl)
import Yoga.JSON (class ReadForeign, class ReadForeignVariant, readImpl, readVariantImpl)


data NoParams = NoParams -- a.k.a. Unit


instance ReadForeign NoParams where
    readImpl = const $ except $ Right NoParams


readMatchImpl :: forall row rec a x b c. ReadForeignVariant x row => RowToList row x => RowToList rec b => VariantMatchCases b c (F a) => Row.Union c () row => Proxy row -> Record rec -> Foreign -> F a
readMatchImpl _ rec f =
    (readImpl f :: F (Variant row))
        >>= Variant.match rec :: F a