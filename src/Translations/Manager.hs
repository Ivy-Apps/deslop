module Translations.Manager where

import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effects.AI
import Translations.Parser
import Translations.Translator

-- | Identity op for successful translations
fixTranslations :: (AI :> es) => Translations -> Eff es (Either Text Translations)
fixTranslations (Translations b xs) =
    fmap (fmap (Translations b) . sequenceA)
        . traverse (fixTranslation b)
        $ xs

fixTranslation :: (AI :> es) => Translation -> Translation -> Eff es (Either Text Translation)
fixTranslation base target =
    translate (base.language, target.language) missing
        <&> fmap rebuild
  where
    baseMap = flatten base.tree
    targetMap = flatten target.tree

    missing :: [(Text, Text)]
    missing = HM.toList $ baseMap `HM.difference` targetMap

    rebuild :: [(Text, Text)] -> Translation
    rebuild newTs =
        target
            { tree = apply (HM.fromList newTs <> targetMap) base.tree
            }

    apply :: HashMap Text Text -> TransTree -> TransTree
    apply m = fkmap (\k v -> maybe v id (HM.lookup k m))

-- | Flattens a tree into dot-separated paths
flatten :: TransTree -> HashMap Text Text
flatten = HM.fromList . go ""
  where
    go p (Leaf k v) = [(p <.> k, v)]
    go p (Root ts) = concatMap (go p) ts
    go p (Branch k ts) = concatMap (go $ p <.> k) ts
