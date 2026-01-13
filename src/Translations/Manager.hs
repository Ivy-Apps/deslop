module Translations.Manager where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Effectful
import Effects.AI
import Translations.Parser

fixTranslations :: (AI :> es) => Translations -> Eff es (Either Text Translations)
fixTranslations = pure . Right . id

fixTranslation :: (AI :> es) => Translation -> Translation -> Eff es (Either Text Translation)
fixTranslation base target = do
    let tM = flatten target.tree
    pure $ Left "WIP"
  where
    findNotTranslated :: TransTree -> HashMap Text Text -> [(Text, Text)]
    findNotTranslated bT tM = []

flatten :: TransTree -> HashMap Text Text
flatten = HM.fromList . go ""
  where
    go :: Text -> TransTree -> [(Text, Text)]
    go p (Leaf k v) = [(joinKey p k, v)]
    go p (Branch k t) = concat $ fmap (go (joinKey p k)) t
    go p (Root t) = concat $ fmap (go p) t

joinKey :: Text -> Text -> Text
joinKey "" k = k
joinKey p k = p <> "." <> k