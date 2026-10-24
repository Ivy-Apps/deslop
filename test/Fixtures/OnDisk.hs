{- | Where the fixtures on disk live, relative to the repository root the suite
runs from.

@fixtures/@ is laid out by language, then by kind; what no language owns sits
under @fixtures/shared/@. Every spec reaches a fixture through one of these, so
moving a directory is a one-line change here rather than a hunt through the
suite.
-}
module Fixtures.OnDisk (
    -- * Per language
    Language (..),
    languageDir,
    projectIn,

    -- * TypeScript
    typescriptFiles,
    typescriptConfigs,
    resolutionCorpus,

    -- * Haskell
    ghcCorpus,

    -- * Nix
    nixCorpus,

    -- * Language-agnostic
    rulebooks,
    staticTree,
) where

import FileSystem.Path (encodeOsPathString)
import System.OsPath (OsPath, osp, (</>))

{- | A language the fixtures hold whole projects in. Its fixtures sit under
@fixtures/\<dir\>/@, and the goldens of its projects under @.golden/\<dir\>/@.
-}
data Language = TypeScript | Haskell | Nix

-- | The directory a language's fixtures and goldens sit in.
languageDir :: Language -> String
languageDir = \case
    TypeScript -> "typescript"
    Haskell -> "haskell"
    Nix -> "nix"

{- | One of the language's whole projects, by its directory name: a directory
holding the language's project config (@tsconfig.json@, a @.cabal@ file, a
@flake.nix@).
-}
projectIn :: Language -> String -> OsPath
projectIn language = ((fixturesOf language </> [osp|projects|]) </>) . encodeOsPathString

-- | Single @.ts@ source files, read on their own.
typescriptFiles :: OsPath
typescriptFiles = fixturesOf TypeScript </> [osp|files|]

-- | Standalone @tsconfig.json@ files.
typescriptConfigs :: OsPath
typescriptConfigs = fixturesOf TypeScript </> [osp|config|]

-- | What @tsc@ resolves each hand-authored case to. See @docs/adr/0018@.
resolutionCorpus :: OsPath
resolutionCorpus = fixturesOf TypeScript </> [osp|resolution-corpus.json|]

-- | What @ghc -M@ says each hand-authored case depends on. See @docs/adr/0022@.
ghcCorpus :: OsPath
ghcCorpus = fixturesOf Haskell </> [osp|ghc-corpus.json|]

-- | What @nix-instantiate --parse@ reads each hand-authored case as. See @docs/adr/0024@.
nixCorpus :: OsPath
nixCorpus = fixturesOf Nix </> [osp|nix-corpus.json|]

-- | Rulebook files, valid and broken, read on their own.
rulebooks :: OsPath
rulebooks = shared </> [osp|rulebook|]

-- | A small directory tree with nothing in it but names.
staticTree :: OsPath
staticTree = shared </> [osp|static|]

fixturesOf :: Language -> OsPath
fixturesOf = (fixtures </>) . encodeOsPathString . languageDir

shared :: OsPath
shared = fixtures </> [osp|shared|]

fixtures :: OsPath
fixtures = [osp|fixtures|]
