{- | What the Nix ecosystem generates and no run should walk: the @result@
links @nix build@ leaves behind, @result-dev@ and the like for a derivation's
other outputs.

On top of "Git.Ignore.alwaysIgnored", which already holds @.direnv@.
-}
module Frontend.Nix.Ignore (nixEcosystemIgnored) where

import Data.Text qualified as T
import FileSystem.Iterator (Entry (..))
import FileSystem.Path (AbsPath (..), decodeOsPath)
import System.OsPath (takeFileName)

-- | @result@ exactly, or @result-@ and anything: @results@ is walked.
nixEcosystemIgnored :: Entry -> Bool
nixEcosystemIgnored entry = name == "result" || "result-" `T.isPrefixOf` name
  where
    name = decodeOsPath . takeFileName $ entry.path.osPath
