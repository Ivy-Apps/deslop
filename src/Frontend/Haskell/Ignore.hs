{- | What the Haskell ecosystem generates and no run should walk: build output
and tool caches.

On top of "Git.Ignore.alwaysIgnored", which holds what is never walked in any
language.
-}
module Frontend.Haskell.Ignore (
    hsEcosystemIgnored,
    hsEcosystemIgnoredNames,
) where

import Data.Set qualified as Set
import FileSystem.Iterator (Entry (..))
import FileSystem.Path (AbsPath (osPath), encodeOsPath)
import System.OsPath (OsPath, takeFileName)

hsEcosystemIgnoredNames :: [Text]
hsEcosystemIgnoredNames =
    [ "dist-newstyle"
    , "dist"
    , ".stack-work"
    , ".hie"
    , ".hpc"
    , ".cabal-sandbox"
    ]

hsEcosystemIgnoredPaths :: Set OsPath
hsEcosystemIgnoredPaths = Set.fromList . fmap encodeOsPath $ hsEcosystemIgnoredNames

-- | Matches the name exactly: @distribution@ is walked.
hsEcosystemIgnored :: Entry -> Bool
hsEcosystemIgnored = (`Set.member` hsEcosystemIgnoredPaths) . takeFileName . (.osPath) . (.path)
