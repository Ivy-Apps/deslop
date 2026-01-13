module Translations.Manager where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Effectful
import Effects.AI
import Translations.Parser

translate :: (AI :> es) => Translations -> Eff es (Either Text Translations)
translate = pure . Right . id

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