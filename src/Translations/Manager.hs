module Translations.Manager where

import Data.Bifunctor (Bifunctor (first))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Maybe
import Data.Text (Text)
import Translations.Parser

flatten :: TransTree -> HashMap Text Text
flatten = HM.fromList . go ""
  where
    go :: Text -> TransTree -> [(Text, Text)]
    go k (Leaf v) = [(k, v)]
    go _ Empty = []
    go p (Branch m) = foldl' step [] (assocsWithParent p m)
      where
        step acc (k, t) = go k t <> acc

apply :: HashMap Text Text -> TransTree -> TransTree
apply m' rt = ud "" rt
  where
    m'keys = HM.keys m'

    ud :: Text -> TransTree -> TransTree
    ud _ Empty = Empty
    ud k (Leaf _) = maybe Empty Leaf (HM.lookup k m')
    ud p (Branch m) = Branch (OM.fromList . mapMaybe step $ assocsWithParent p m)
      where
        step :: (Text, TransTree) -> Maybe (Text, TransTree)
        step (k, t) = undefined

assocsWithParent :: Text -> OMap Text TransTree -> [(Text, TransTree)]
assocsWithParent p = fmap (first (joinKey p)) . OM.assocs

joinKey :: Text -> Text -> Text
joinKey "" k = k
joinKey p' k = p' <> "." <> k