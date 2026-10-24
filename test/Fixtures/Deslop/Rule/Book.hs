{- | Rulebooks loaded from their source text the way a run loads them, through
"Deslop.Rule.Book.Loader", so that a spec's rulebook is parsed, desugared and
compiled exactly as an author's would be.
-}
module Fixtures.Deslop.Rule.Book (
    loadRulebookBytes,
    rulebookOf,
    rulebookFrom,
) where

import Deslop.Rule.Book (Rulebook)
import Deslop.Rule.Book.Loader (RulebookLoadError, loadRulebookFromFile)
import Doubles.FileSystem (mockFileAt, runMockRoFileSystem)
import Effectful (runPureEff)
import Fixtures.Deslop.Rule.Book.Dto (rulebookYamlOf)
import TestUtils (ap)

-- | One rulebook file's bytes, loaded.
loadRulebookBytes :: ByteString -> Either RulebookLoadError Rulebook
loadRulebookBytes bytes =
    runPureEff . runMockRoFileSystem (mockFileAt path bytes) $ loadRulebookFromFile path
  where
    path = ap "/project/deslop/rules/rulebook.yaml"

-- | The rulebook of that id holding these rules, each written as 'rulebookYamlOf' takes it.
rulebookOf :: Text -> [[Text]] -> Either RulebookLoadError Rulebook
rulebookOf rulebookId = loadRulebookBytes . rulebookYamlOf rulebookId

-- | As 'rulebookOf', for a rulebook the spec relies on being valid.
rulebookFrom :: (HasCallStack) => Text -> [[Text]] -> Rulebook
rulebookFrom rulebookId =
    either (error . ("rulebook fixture does not load: " <>) . show) id . rulebookOf rulebookId
