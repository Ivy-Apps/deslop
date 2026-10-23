{- | What the JavaScript ecosystem generates and no run should walk:
dependencies, build output and tool caches.

On top of "Git.Ignore.alwaysIgnored", which holds what is never walked in any
language. Pruned greedily for the same reason: these trees are large enough
that walking them costs more than every other part of a run combined.
-}
module Frontend.TypeScript.Ignore (
    jsEcosystemIgnored,
    jsEcosystemIgnoredNames,
) where

import Data.Set qualified as Set
import FileSystem.Iterator (Entry (..))
import FileSystem.Path (AbsPath (osPath), encodeOsPath)
import System.OsPath (OsPath, takeFileName)

jsEcosystemIgnoredNames :: [Text]
jsEcosystemIgnoredNames =
    [ "node_modules"
    , "dist"
    , ".next"
    , "next-env.d.ts"
    , ".next-env.d.ts"
    , "build"
    , "out"
    , ".output"
    , "storybook-static"
    , "coverage"
    , ".turbo"
    , ".parcel-cache"
    , ".yarn"
    , ".svelte-kit"
    , ".nuxt"
    , ".astro"
    , ".vercel"
    , ".wrangler"
    ]

jsEcosystemIgnoredPaths :: Set OsPath
jsEcosystemIgnoredPaths = Set.fromList . fmap encodeOsPath $ jsEcosystemIgnoredNames

-- | Matches the name exactly: @node_modules_backup@ is walked.
jsEcosystemIgnored :: Entry -> Bool
jsEcosystemIgnored = (`Set.member` jsEcosystemIgnoredPaths) . takeFileName . (.osPath) . (.path)
