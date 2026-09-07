{- | The differential test: Deslop must resolve a specifier to the same file
TypeScript does.

The answers in @fixtures\/resolution-corpus.json@ are the real compiler's, taken
by @just update-resolution-corpus@ and committed - so this runs against an
oracle without needing @tsc@, or even node, on the machine. See
@docs\/adr\/0018-typescript-resolution-is-judged-against-tsc.md@.

A case carrying a @knownGap@ is one Deslop does not yet resolve the way @tsc@
does. Those are asserted to /still differ/: closing a gap turns its own test red
and names what changed, rather than the gap quietly staying invisible.
-}
module TypeScript.ResolutionCorpusSpec (spec) where

import Data.Aeson (FromJSON, Value, eitherDecodeFileStrict', encode)
import Data.Map.Strict qualified as Map
import Deslop.AST (moduleNameUnsafe)
import Doubles.FileSystem (mockFileSystem, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import FileSystem.Path (AbsPath, ProjectRoot (..), absPathUnsafe, decodeOsPath, encodeOsPath, portablePath, relativePathTo)
import Test.Hspec
import TestUtils (fixturesPath, requireRight)
import TypeScript.Config (TsConfig)
import TypeScript.Config.Loader (loadTsConfig)
import TypeScript.ModuleResolver (resolve)

-- | One case, exactly as authored and recorded in the corpus file.
data ResolutionCase = ResolutionCase
    { name :: Text
    , configs :: Map Text Value
    , files :: [Text]
    , from :: Text
    , specifier :: Text
    , tsc :: Maybe Text
    -- ^ The compiler's answer. 'Nothing' is "not resolved".
    , knownGap :: Maybe Text
    -- ^ Present only where Deslop is known to differ from 'tsc'.
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

newtype Corpus = Corpus {cases :: [ResolutionCase]}
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

spec :: Spec
spec = describe "TypeScript.ResolutionCorpus" $ do
    corpus <- runIO loadCorpus
    for_ corpus.cases $ \c ->
        it (toString (label c)) $ do
            actual <- resolveCase c
            case c.knownGap of
                Nothing -> actual `shouldBe` c.tsc
                Just _ -> actual `shouldNotBe` c.tsc
  where
    label c = c.name <> " (" <> c.specifier <> ")" <> maybe "" (const " [known gap]") c.knownGap

{- | What Deslop resolves the case's specifier to, spelled from the case root so
that it can be compared with what @tsc@ recorded.
-}
resolveCase :: ResolutionCase -> IO (Maybe Text)
resolveCase c = do
    cfg <- caseConfig c
    resolved <-
        runEff
            . runMockRoFileSystem (mockFileSystem (caseFileSystem c))
            . runReader @TsConfig cfg
            $ resolve (pathIn c.from) (moduleNameUnsafe c.specifier)
    pure $ do
        absPath <- resolved
        -- resolve answers for a path whether or not anything is there, so a
        -- specifier naming nothing has to be read as unresolved.
        guard (absPath `elem` map pathIn c.files)
        pure . portablePath . relativePathTo caseRoot $ absPath

-- | The config the case declares, loaded through the real @extends@ chain.
caseConfig :: ResolutionCase -> IO TsConfig
caseConfig c = do
    loaded <-
        runEff
            . runMockRoFileSystem (mockFileSystem (caseFileSystem c))
            $ loadTsConfig (pathIn "tsconfig.json")
    fst <$> requireRight (\e -> "loading tsconfig for " <> toString c.name <> ": " <> show e) loaded

{- | Every file the case says exists: its configs with their bytes, and its
sources, which are empty because resolution never reads them.
-}
caseFileSystem :: ResolutionCase -> [(AbsPath, ByteString)]
caseFileSystem c =
    [(pathIn path, toStrict (encode config)) | (path, config) <- Map.toList c.configs]
        <> [(pathIn f, "") | f <- c.files]

caseRoot :: ProjectRoot
caseRoot = ProjectRoot (absPathUnsafe (encodeOsPath "/corpus"))

pathIn :: Text -> AbsPath
pathIn path = absPathUnsafe . encodeOsPath $ "/corpus/" <> path

loadCorpus :: IO Corpus
loadCorpus = do
    let path = fixturesPath
    decoded <- eitherDecodeFileStrict' (toString (decodeOsPath path) <> "/resolution-corpus.json")
    requireRight ("reading the resolution corpus: " <>) decoded
