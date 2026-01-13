module Translations.Manager where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Effectful
import Effects.AI
import Translations.Parser
import Translations.Translator

fixTranslations :: (AI :> es) => Translations -> Eff es (Either Text Translations)
fixTranslations = pure . Right . id

fixTranslation :: (AI :> es) => Translation -> Translation -> Eff es (Either Text Translation)
fixTranslation base target = do
    let tM = flatten target.tree
    let missingTs = findNotTranslated "" tM base.tree
    tsRes <- fmap HM.fromList <$> translate (base.language, target.language) missingTs
    case tsRes of
        Right fixedTs -> do
            pure . Right $ target {tree = apply (fixedTs <> tM) base.tree}
        Left e -> pure . Left $ e
  where
    findNotTranslated :: Text -> HashMap Text Text -> TransTree -> [(Text, Text)]
    findNotTranslated p tM (Leaf k v)
        | joinKey p k `HM.member` tM = []
        | otherwise = [(joinKey p k, v)]
    findNotTranslated p tM (Root ts) = concatMap (findNotTranslated p tM) $ ts
    findNotTranslated p tM (Branch k ts) = concatMap (findNotTranslated (joinKey p k) tM) $ ts

    apply :: HashMap Text Text -> TransTree -> TransTree
    apply m t = t

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