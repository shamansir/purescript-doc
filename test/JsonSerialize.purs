module Test.JsonSerialize where

import Prelude

import Data.Maybe (Maybe(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Text.Output.Blessed (multiLine) as T
import Data.Text.Format as F
-- import Data.Text.Doc as D
import Data.Text.Doc as D
import Data.Text.Doc ((<+>), (</>))
import Data.String as String
import Data.Array (concat, take, fromFoldable) as Array
import Data.List (List, (:))
import Data.List (List(..)) as List

import Data.Text.Format.Org.Types (Check(..))

import Yoga.JSON (read_, write, writeJSON)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)



spec :: Spec Unit
spec = do

  describe "JSON serialization" $

    it "properly encodes variants" $ do
        (writeJSON Check) `shouldNotEqual` (writeJSON Uncheck)
        (writeJSON Check) `shouldNotEqual` (writeJSON Halfcheck)
        (writeJSON Uncheck) `shouldNotEqual` (writeJSON Halfcheck)
        (read_ $ write Check) `shouldEqual` (Just Check)
        (read_ $ write Uncheck) `shouldEqual` (Just Uncheck)
        (read_ $ write Halfcheck) `shouldEqual` (Just Halfcheck)