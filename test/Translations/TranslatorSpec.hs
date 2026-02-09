module Translations.TranslatorSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Translations.Translator

enMessages :: [(Text, Text)]
enMessages =
    [ ("errors.title", "Something went wrong")
    , ("errors.message", "Try again or contact support.")
    , ("errors.cta", "Try again")
    ]

spec :: Spec
spec = do
    describe "Prompt" $ do
        it "EN to ES" $ do
            let p = translatePrompt "en" "es" enMessages
            defaultGolden "prompt-en-es" (T.unpack p)
