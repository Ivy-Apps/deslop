{- | The differential test: Deslop must find the same dependencies between a
project's Haskell files that GHC does.

The answers in @fixtures\/ghc-corpus.json@ are the real compiler's, taken by
@just update-ghc-corpus@ with @ghc -M@ and committed - so this runs against an
oracle without needing @ghc@ on the machine. See
@docs\/adr\/0022-haskell-dependencies-are-judged-against-ghc.md@.

A case carrying a @knownGap@ is one Deslop does not yet read the way GHC does.
Those are asserted to /still differ/: closing a gap turns its own test red.
-}
module Frontend.Haskell.GhcCorpusSpec (spec) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deslop.Module (DependencyEdge (..), EdgeTarget (..), Module (..), ModuleId (..))
import FileSystem.Path (decodeOsPath, portablePath)
import Fixtures.Frontend.Haskell.Project (Lowered (..), lowerFiles)
import Renderable qualified
import Test.Hspec
import TestUtils (fixturesPath, requireRight)

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

newtype Corpus = Corpus {cases :: [GhcCase]}
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

spec :: Spec
spec = describe "Frontend.Haskell.GhcCorpus" $ do
    corpus <- runIO loadCorpus
    for_ corpus.cases $ \c ->
        it (toString (c.name <> maybe "" (const " [known gap]") c.knownGap)) $ do
            actual <- dependenciesOf c
            case c.knownGap of
                Nothing -> actual `shouldBe` c.ghc
                Just _ -> actual `shouldNotBe` c.ghc

{- | Every dependency Deslop finds between two files of the case, spelled from
the case root, as a set: GHC does not say how often a file is imported, nor
whether it is re-exported.
-}
dependenciesOf :: GhcCase -> IO [(Text, Text)]
dependenciesOf c = do
    lowered <- requireRight (toString . Renderable.render) =<< lowerFiles (cabal : fmap (second encodeUtf8) (Map.toList c.files))
    pure . sortNub $
        [ (portablePath m.path, fromRoot targetId)
        | m <- lowered.modules
        , DependencyEdge {target = Resolved targetId _} <- m.edges
        ]
  where
    cabal = ("deslop-corpus.cabal", "cabal-version: 3.0\nname: corpus\nversion: 0\nlibrary\n  hs-source-dirs: src\n")
    fromRoot moduleId = fromMaybe moduleId.text (T.stripPrefix "/project/" moduleId.text)

loadCorpus :: IO Corpus
loadCorpus = do
    decoded <- eitherDecodeFileStrict' (toString (decodeOsPath fixturesPath) <> "/ghc-corpus.json")
    requireRight ("reading the ghc corpus: " <>) decoded
