module Translations.Translator where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effects.AI
import Translations.Parser
import Data.Aeson.Encode.Pretty
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE

translate ::
    (AI :> es) =>
    (LangCode, LangCode) ->
    [(Text, Text)] ->
    Eff es (Either Text [(Text, Text)])
translate (from, to) ts = pure . Right $ ts

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
    lang l = T.toUpper l
    buildJson = TL.toStrict . TLE.decodeUtf8 . encodePretty . M.fromList $ ts

