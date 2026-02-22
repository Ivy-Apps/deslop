module Translations.Translator (
    translate,
    translatePrompt,
    parseTranslateResponse,
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Void
import Effectful
import Effects.AI
import Text.Megaparsec
import Text.Megaparsec.Char
import Translations.Parser

type Parser = Parsec Void Text

translate ::
    (AI :> es) =>
    (LangCode, LangCode) ->
    [(Text, Text)] ->
    Eff es (Either Text [(Text, Text)])
translate _ [] = pure . Right $ []
translate (from, to) ts =
    prompt FastLLM (translatePrompt from to ts)
        >>= pure . (parseTranslateResponse <=< first mapAiError)
  where
    mapAiError :: AIError -> Text
    mapAiError = T.pack . show

translatePrompt :: LangCode -> LangCode -> [(Text, Text)] -> Text
translatePrompt from to ts =
    "Translate from "
        <> lang from
        <> " to "
        <> lang to
        <> " the following NextJS-Intl JSON messsages:\n"
        <> "```json\n"
        <> buildJson
        <> "\n```"
        <> "\n\nReturn the same JSON translated in "
        <> lang to
        <> "."
  where
    lang = T.toUpper
    buildJson = TL.toStrict . TLE.decodeUtf8 . encodePretty . M.fromList $ ts

parseTranslateResponse :: Text -> Either Text [(Text, Text)]
parseTranslateResponse = extractJson >=> bimap T.pack M.toList . decodeTranslations
  where
    decodeTranslations :: TL.Text -> Either String (Map Text Text)
    decodeTranslations = eitherDecode . TLE.encodeUtf8

    extractJson = bimap (T.pack . errorBundlePretty) TL.pack . runParser jsonParser ""

    jsonParser :: Parser String
    jsonParser = manyTill anySingle (string "```json") *> manyTill anySingle (string "```")
