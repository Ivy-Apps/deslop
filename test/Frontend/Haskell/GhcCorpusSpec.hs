{- | The differential test: Deslop must find the same dependencies between a
project's Haskell files that GHC does.

The answers in @fixtures\/haskell\/ghc-corpus.json@ are the real compiler's, taken by
@just update-ghc-corpus@ with @ghc -M@ and committed - so this runs against an
oracle without needing @ghc@ on the machine. See
@docs\/adr\/0022-haskell-dependencies-are-judged-against-ghc.md@.

A case carrying a @knownGap@ is one Deslop does not yet read the way GHC
does; see 'TestUtils.corpusCase'.
-}
module Frontend.Haskell.GhcCorpusSpec (spec) where

import Data.Aeson (FromJSON)
import Data.Map.Strict qualified as Map
import Deslop.Module (DependencyEdge (..), EdgeTarget (..), Module (..), ModuleId (..))
import FileSystem.Path (portablePath)
import Fixtures.FileSystem.Path (fromProjectText)
import Fixtures.Frontend.Haskell.Project (Lowered (..), lowerFiles)
import Fixtures.OnDisk (ghcCorpus)
import Renderable qualified
import Test.Hspec
import TestUtils (corpusCase, loadCorpus, requireRight)

-- | One case, exactly as authored and recorded in the corpus file.
data GhcCase = GhcCase
    { name :: Text
    , files :: Map Text Text
    , ghc :: [(Text, Text)]
    -- ^ The compiler's answer: every @(importer, imported)@ pair of files.
    , knownGap :: Maybe Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

spec :: Spec
spec = describe "Frontend.Haskell.GhcCorpus" $ do
    cases <- runIO (loadCorpus @GhcCase ghcCorpus)
    for_ cases $ \c ->
        corpusCase c.name c.knownGap (dependenciesOf c) c.ghc

{- | Every dependency Deslop finds between two files of the case, spelled from
the case root, as a set: GHC does not say how often a file is imported, nor
whether it is re-exported.
-}
dependenciesOf :: GhcCase -> IO [(Text, Text)]
dependenciesOf c = do
    lowered <- requireRight (toString . Renderable.render) =<< lowerFiles (cabal : fmap (second encodeUtf8) (Map.toList c.files))
    pure . sortNub $
        [ (portablePath m.path, fromProjectText targetId.text)
        | m <- lowered.modules
        , DependencyEdge {target = Resolved targetId _} <- m.edges
        ]
  where
    cabal = ("deslop-corpus.cabal", "cabal-version: 3.0\nname: corpus\nversion: 0\nlibrary\n  hs-source-dirs: src\n")
