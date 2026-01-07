module Translations.Manager where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Text (Text)
import Translations.Parser

flatten :: TransTree -> HashMap Text Text
flatten = HM.fromList . go ""
  where
    go :: Text -> TransTree -> [(Text, Text)]
    go k (Leaf v) = [(k, v)]
    go p (Branch m) = foldl' step [] (OM.assocs m)
      where
        step acc (k, t) = go (key p k) t <> acc

        key "" k = k
        key p' k = p' <> "." <> k