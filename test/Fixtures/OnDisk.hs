{- | Where the fixtures on disk live, relative to the repository root the suite
runs from.

@fixtures/@ is laid out by language, then by kind; what no language owns sits
under @fixtures/shared/@. Every spec reaches a fixture through one of these, so
moving a directory is a one-line change here rather than a hunt through the
suite.
-}
module Fixtures.OnDisk (
    -- * TypeScript
    typescriptProjects,
    typescriptProject,
    typescriptFiles,
    typescriptConfigs,
    resolutionCorpus,

    -- * Haskell
    haskellProjects,
    haskellProject,
    ghcCorpus,

    -- * Language-agnostic
    rulebooks,
    staticTree,
) where

import FileSystem.Path (encodeOsPathString)
import System.OsPath (OsPath, osp, (</>))

-- | Whole TypeScript projects, each a directory with a @tsconfig.json@.
typescriptProjects :: OsPath
typescriptProjects = [osp|fixtures/typescript/projects|]

-- | One of 'typescriptProjects', by its directory name.
typescriptProject :: String -> OsPath
typescriptProject = (typescriptProjects </>) . encodeOsPathString

-- | Single @.ts@ source files, read on their own.
typescriptFiles :: OsPath
typescriptFiles = [osp|fixtures/typescript/files|]

-- | Standalone @tsconfig.json@ files.
typescriptConfigs :: OsPath
typescriptConfigs = [osp|fixtures/typescript/config|]

-- | What @tsc@ resolves each hand-authored case to. See @docs/adr/0018@.
resolutionCorpus :: OsPath
resolutionCorpus = [osp|fixtures/typescript/resolution-corpus.json|]

-- | Whole Haskell projects, each a directory with a @.cabal@ file.
haskellProjects :: OsPath
haskellProjects = [osp|fixtures/haskell/projects|]

-- | One of 'haskellProjects', by its directory name.
haskellProject :: String -> OsPath
haskellProject = (haskellProjects </>) . encodeOsPathString

-- | What @ghc -M@ says each hand-authored case depends on. See @docs/adr/0022@.
ghcCorpus :: OsPath
ghcCorpus = [osp|fixtures/haskell/ghc-corpus.json|]

-- | Rulebook files, valid and broken, read on their own.
rulebooks :: OsPath
rulebooks = [osp|fixtures/shared/rulebook|]

-- | A small directory tree with nothing in it but names.
staticTree :: OsPath
staticTree = [osp|fixtures/shared/static|]
