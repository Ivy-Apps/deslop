module Translations.Translator where

import Data.Text
import Effectful
import Effects.AI
import Translations.Parser

translate ::
    (AI :> es) =>
    (Language, Language) ->
    [(Text, Text)] ->
    Eff es (Either Text [(Text, Text)])
translate (from, to) ts = pure . Right $ ts