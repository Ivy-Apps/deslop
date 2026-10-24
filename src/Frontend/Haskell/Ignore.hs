{- | What the Haskell ecosystem generates and no run should walk: build output
and tool caches.

On top of "Git.Ignore.alwaysIgnored", which holds what is never walked in any
language.
-}
module Frontend.Haskell.Ignore (hsEcosystemIgnored) where

import FileSystem.Iterator (Entry)
import Git.Ignore (ignoredByName)

hsEcosystemIgnoredNames :: [Text]
hsEcosystemIgnoredNames =
    [ "dist-newstyle"
    , "dist"
    , ".stack-work"
    , ".hie"
    , ".hpc"
    , ".cabal-sandbox"
    ]

-- | Matches the name exactly: @distribution@ is walked.
hsEcosystemIgnored :: Entry -> Bool
hsEcosystemIgnored = ignoredByName hsEcosystemIgnoredNames
