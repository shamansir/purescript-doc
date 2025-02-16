module Data.Text.Diff where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)

import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Array (catMaybes) as Array
-- import Data.Text.Format (of_)
import Data.Tuple (Tuple)
import Data.Tuple (fst, snd) as Tuple
import Data.Bifunctor (bimap)
import Data.These (these, These(..), theseLeft, theseRight)
import Data.Align (aligned, crosswalk)



class Eq a <= Diffable a where
    toDiffString :: a -> String


diffCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
diffCompare v1 v2 =
  when (v1 /= v2) $
    liftEffect $ throw $ lineByLineComparison (toDiffString v1) (toDiffString v2)


diffStackCompare
  :: forall m t
   . MonadEffect m
  => MonadThrow Error m
  => Diffable t
  => t
  -> t
  -> m Unit
diffStackCompare v1 v2 =
  when (v1 /= v2) $
    liftEffect $ throw $ twoStacksComparison (toDiffString v1) (toDiffString v2)


lineByLineComparison :: String -> String -> String
lineByLineComparison a b =
    String.joinWith "\n" $ toDiffString <$> compareByLines a b



twoStacksComparison :: String -> String -> String
twoStacksComparison a b =
    let
        comparison = compareByLines a b
        formatLeft = case _ of
            This lA -> Just $ "++ " <> lA
            That _ -> Nothing
            Both lA lB ->
                Just $ if lA == lB
                        then ".. " <> lA
                        else ">> " <> lA
        formatRight = case _ of
            This _ -> Nothing
            That lB -> Just $ "-- " <> lB
            Both lA lB ->
                Just $ if lA == lB
                        then ".. " <> lB
                        else "<< " <> lB

    in
           (String.joinWith "\n" $ Array.catMaybes $ formatLeft  <$> comparison)
        <> "\n---------------------------------------------------------------\n"
        <> (String.joinWith "\n" $ Array.catMaybes $ formatRight <$> comparison)


compareMany :: forall a. Array a -> Array a -> Array (These a a)
compareMany = aligned


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
