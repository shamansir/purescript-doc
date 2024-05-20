module Data.Text.Format.Org.Keywords where

import Prelude

import Foreign (Foreign, F)

import Data.Array as Array
import Data.Map (Map)
import Data.Map (empty, insert, size, fromFoldable, toUnfoldable, isEmpty) as Map
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign)


newtype KeywordsOf k v = KeywordsOf (Map (Int /\ k) v)
type OrderedKeywordsOf k v = Array (k /\ v)
type JsonKeywordsOf k v = Array (Int /\ k /\ v)


type Keywords = KeywordsOf String String
type OrderedKeyword = OrderedKeywordsOf String String
type JsonKeywords = JsonKeywordsOf String String


derive newtype instance Functor (KeywordsOf k)
derive instance Newtype (KeywordsOf k v) _

{- instance (ReadForeign k, ReadForeign v) => ReadForeign (KeywordsOf k v)
    where readImpl f = (readImpl f :: F (Array (Int /\ (k /\ v)))) <#> toKeywords -}


empty :: forall k v. KeywordsOf k v
empty = wrap Map.empty


hasKeywords :: forall k v. KeywordsOf k v -> Boolean
hasKeywords = not isEmpty


isEmpty :: forall k v. KeywordsOf k v -> Boolean
isEmpty = unwrap >>> Map.isEmpty


push :: forall k v. Ord k => k -> v -> KeywordsOf k v -> KeywordsOf k v
push k v = unwrap >>> putLast >>> wrap
    where putLast map = Map.insert (Map.size map /\ k) v map


insert :: forall k v. Ord k => Int -> k -> v -> KeywordsOf k v -> KeywordsOf k v
insert n k v = unwrap >>> Map.insert (n /\ k) v >>> wrap


toKeywords :: forall k v. Ord k => JsonKeywordsOf k v -> KeywordsOf k v
toKeywords = wrap <<< Map.fromFoldable <<< map _swap3


toKeywords' :: forall k v. Ord k => OrderedKeywordsOf k v -> KeywordsOf k v
toKeywords' = toKeywords <<< Array.mapWithIndex ((/\))


fromKeywords :: forall k v. KeywordsOf k v -> JsonKeywordsOf k v
fromKeywords = map _bswap3 <<< Map.toUnfoldable <<< unwrap


fromKeywords' :: forall k v. KeywordsOf k v -> OrderedKeywordsOf k v
fromKeywords' = fromKeywords >>> map Tuple.snd


-- t a (t b c) ->  t (t a b) c
_swap3 :: forall a b c. a /\ (b /\ c) -> (a /\ b) /\ c
_swap3 (a /\ (b /\ c)) = (a /\ b) /\ c


-- t (t a b) c ->  t a (t b c)
_bswap3 :: forall a b c. (a /\ b) /\ c -> a /\ (b /\ c)
_bswap3 ((a /\ b) /\ c) = a /\ (b /\ c)
