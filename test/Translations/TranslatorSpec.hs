module Translations.TranslatorSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Translations.Translator
import Text.Show.Pretty (ppShow)
import System.FilePath
import TestUtils

enMessages :: [(Text, Text)]
enMessages =
    [ ("errors.title", "Something went wrong")
    , ("errors.message", "Try again or contact support.")
    , ("errors.cta", "Try again")
    ]

fixturesPath :: FilePath
fixturesPath = "test/fixtures/translations"

responseFixturesPath :: FilePath
responseFixturesPath = fixturesPath </> "responses"

spec :: Spec
spec = do
    describe "Prompt" $ do
        it "EN to ES" $ do
            let p = translatePrompt "en" "es" enMessages
            defaultGolden "prompt-en-es" (T.unpack p)

    describe "Parses LLM response" $ do
        (runIO $ listFixtures responseFixturesPath ".txt")
            >>= mapM_ parseGoldenTest
  where
    parseGoldenTest :: FilePath -> Spec
    parseGoldenTest fname = do
        let testName = takeBaseName fname
        it testName $ do 
          response <- TIO.readFile (responseFixturesPath </> fname)
          let res = parseTranslateResponse response
          return $ defaultGolden ("parse-" <> testName) (fmtRes res)


fmtRes :: Either Text [(Text, Text)] -> String
fmtRes (Left e) = "Left: " <> T.unpack e
fmtRes (Right ts) = unlines . ("Right: " :) . fmap fmtPair $ ts 
  where
    fmtPair (k, v) = "  " <> T.unpack k <> ": " <> T.unpack v  
