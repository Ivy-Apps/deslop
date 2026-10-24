{- | Rulebooks loaded from their source text the way a run loads them, through
"Deslop.Rule.Book.Loader", so that a spec's rulebook is parsed, desugared and
compiled exactly as an author's would be.
-}
module Fixtures.Deslop.Rule.Book (
    loadRulebookBytes,
) where

import Deslop.Rule.Book (Rulebook)
import Deslop.Rule.Book.Loader (RulebookLoadError, loadRulebookFromFile)
import Doubles.FileSystem (mockFileAt, runMockRoFileSystem)
import Effectful (runPureEff)
import Fixtures.FileSystem.Path (inProject)

-- | One rulebook file's bytes, loaded.
loadRulebookBytes :: ByteString -> Either RulebookLoadError Rulebook
loadRulebookBytes bytes =
    runPureEff . runMockRoFileSystem (mockFileAt path bytes) $ loadRulebookFromFile path
  where
    path = inProject "deslop/rules/rulebook.yaml"
