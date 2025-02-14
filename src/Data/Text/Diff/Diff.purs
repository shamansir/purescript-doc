module Data.Text.Diff where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)

import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple (fst, snd) as Tuple
import Data.Bifunctor (bimap)

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Data.These (these, These)
import Data.Align (align)


class Eq a <= Diffable a where
    toDiffString :: a -> String


ensureEqual
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
ensureEqual v1 v2 =
  when (v1 /= v2) $
    liftEffect $ throw $ printLineComparison (toDiffString v1) (toDiffString v2)


printLineComparison :: String -> String -> String
printLineComparison a b =
    String.joinWith "\n" $ toDiffString <$> compareByLines a b


compareMany :: forall a. Array a -> Array a -> Array (These a a)
compareMany = align identity


compareByLines :: String -> String -> Array (These String String)
compareByLines a b =
    let
        linesA = String.split (String.Pattern "\n") a
        linesB = String.split (String.Pattern "\n") b
    in compareMany linesA linesB


instance Diffable String where
    toDiffString = identity


instance Diffable Int where
    toDiffString = show


instance (Diffable a, Diffable b) => Diffable (These a b) where
    toDiffString = bimap toDiffString toDiffString >>>
        these
            (\lA -> "++ " <> lA)
            (\lB -> "-- " <> lB)
            (\lA lB ->
                if lA == lB then ".. " <> lA
                else ">> " <> lA <> "\n" <> "<< " <> lB
            )
        {-
        these
            (\lA -> "< " <> lA)
            (\lB -> "> " <> lB)
            (\lA lB -> ": " <> lA <> " ; " <> lB)
        -}


instance (Diffable a, Diffable b) => Diffable (Tuple a b) where
    toDiffString tpl = toDiffString (Tuple.fst tpl) <> "\n" <> toDiffString (Tuple.snd tpl)



instance Diffable a => Diffable (Array a) where
    toDiffString arr = String.joinWith "\n" $ toDiffString <$> arr
