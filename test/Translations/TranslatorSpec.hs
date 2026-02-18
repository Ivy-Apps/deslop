module Translations.TranslatorSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (runEff)
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils
import Text.Show.Pretty (ppShow)
import Translations.Translator

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

    describe "Translate" $ do
        it "does not translate empty messages" $ do
            res <- runEff . runAIAlwaysFail $ translate ("en", "es") []
            res `shouldBe` Right []

    describe "Parses LLM response" $ do
        runIO (listFixtures responseFixturesPath ".txt")
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
