module Test.Org.Export.Json where

import Prelude
import Foreign as F

import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console as Console
import Control.Monad.Error.Class (class MonadThrow)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse_)
-- import Data.Text.Doc as D

import Data.Text.Doc as D
import Data.Text.Format.Org.Types (OrgFile, Check(..))
import Data.Text.Format.Org.Construct as Org
import Data.Text.Format.Org.Render as R

import Yoga.JSON (read_, read, readJSON, write, writeJSON)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotEqual)


import Org.Test.Test01 as Test01
import Org.Test.Test02a as Test02a
import Org.Test.Test02b as Test02b
import Org.Test.Test03a as Test03a
import Org.Test.Test03b as Test03b
import Org.Test.Test03c as Test03c
import Org.Test.Test03d as Test03d
import Org.Test.Test03e as Test03e
import Org.Test.Test04a as Test04a
import Org.Test.Test04b as Test04b
import Org.Test.Test04c as Test04c
import Org.Test.Test04d as Test04d
import Org.Test.Test04e as Test04e
import Org.Test.Test04f as Test04f
import Org.Test.Test04g as Test04g
import Org.Test.Test04h as Test04h
import Org.Test.Test04i as Test04i



spec :: Spec Unit
spec = do

  describe "export to JSON" $ do

    it "properly encodes variants" $ do
        (writeJSON Check) `shouldNotEqual` (writeJSON Uncheck)
        (writeJSON Check) `shouldNotEqual` (writeJSON Halfcheck)
        (writeJSON Uncheck) `shouldNotEqual` (writeJSON Halfcheck)
        (read_ $ write Check) `shouldEqual` (Just Check)
        (read_ $ write Uncheck) `shouldEqual` (Just Uncheck)
        (read_ $ write Halfcheck) `shouldEqual` (Just Halfcheck)

    {-
    it "works with the empty .org files" $ do
        (read_ $ write Org.empty) `shouldEqual` (Just Org.empty)
    -}

    it "works with the empty JSON" $ do
        emptySample <- liftEffect $ readTextFile UTF8 "./test/examples/org-empty.json"
        (writeJSON Org.empty) `shouldEqual` emptySample


    it "01. works with the syntax sample" $
        qjsontest "01-empty" $ Test01.test

    it "02. works with the meta sample (a)" $
        qjsontest "02a-meta" $ Test02a.test

    it "02. works with the meta sample (b)" $
        qjsontest "02b-meta-special" $ Test02b.test

    it "03. works with basic headings and levels (a)" $
        qjsontest "03a-headings-with-no-content" $ Test03a.test        


renderOptions :: D.Options
renderOptions = { break : D.All, indent : D.Spaces 1 }


qjsontest
    :: forall (m ∷ Type -> Type)
     . Bind m => MonadEffect m => MonadThrow _ m
    => String -> OrgFile -> m Unit
qjsontest slugId orgFile = do
    let orgFileJson = writeJSON orgFile
    let eFromJsonTxt = readJSON orgFileJson
    -- liftEffect $ Console.log orgFileJson
    case eFromJsonTxt of 
        Right orgFileFromJsonTxt -> do
            writeJSON orgFileFromJsonTxt `shouldEqual` orgFileJson
            (D.render renderOptions $ R.layout orgFile)
                    `shouldEqual` (D.render renderOptions $ R.layout orgFileFromJsonTxt)
        Left errors -> 
            traverse_ (F.renderForeignError >>> fail) errors
    let eFromJsonF = read $ write orgFile
    case eFromJsonF of 
        Right orgFileFromJsonF -> 
            (D.render renderOptions $ R.layout orgFile)
                    `shouldEqual` (D.render renderOptions $ R.layout orgFileFromJsonF)
        Left errors -> 
            traverse_ (F.renderForeignError >>> fail) errors