{- | Test plumbing: golden helpers, fixture-directory access, and the small
combinators that keep a failing assertion readable.

Deliberately domain-free. A helper that builds a 'Deslop.Module.Module' or a
'Frontend.TypeScript.Config.TsConfig' belongs under @Fixtures.@ next to the module whose
type it builds; a helper that only knows about hspec, Hedgehog or the
filesystem belongs here.
-}
module TestUtils (
    -- * Golden tests
    snapshot,

    -- * Fixtures on disk
    listFixtures,
    copyDir,

    -- * Corpora
    loadCorpus,
    corpusCase,

    -- * Paths
    ap,
    rp,
    mkAbsolute,

    -- * Assertions
    prop,
    requireJust,
    requireRight,
) where

import Control.Exception (throwIO)
import Control.Exception.Base (AssertionFailed (..))
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effects.FileSystem (fsMkAbsolute, runFileSystemIO)
import FileSystem.Path (
    AbsPath,
    ProjectRelativePath,
    absPathUnsafe,
    decodeOsPath,
    encodeOsPath,
    encodeOsPathString,
    relativePathUnsafe,
 )
import Hedgehog (PropertyT)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, takeExtension, (</>))
import Test.Hspec (Spec, expectationFailure, it, shouldBe, shouldNotBe)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Golden tests
--------------------------------------------------------------------------------

snapshot :: OsPath -> [String] -> IO String
snapshot tmpDir filesToVerify = do
    results <- forM filesToVerify $ \relPath -> do
        raw <- SFO.readFile' (tmpDir </> encodeOsPathString relPath)
        let content = TE.decodeUtf8 raw
        let header = "\n\n\n>>> FILE: " <> T.pack relPath <> "\n"
        return $ header <> content
    pure . T.unpack . T.dropWhile (== '\n') $ T.concat results

--------------------------------------------------------------------------------
-- Fixtures on disk
--------------------------------------------------------------------------------

listFixtures :: OsPath -> String -> IO [OsPath]
listFixtures dir ext = do
    files <- SDO.listDirectory dir
    let extOs = encodeOsPathString ext
    pure $ filter (\f -> takeExtension f == extOs) files

copyDir :: OsPath -> OsPath -> IO ()
copyDir src dst = do
    SDO.createDirectoryIfMissing True dst
    content <- SDO.listDirectory src
    forM_ content $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- SDO.doesDirectoryExist srcPath
        if isDirectory
            then copyDir srcPath dstPath
            else SDO.copyFile srcPath dstPath

--------------------------------------------------------------------------------
-- Corpora
--------------------------------------------------------------------------------

-- | A corpus file: its cases, exactly as authored and recorded.
newtype Corpus c = Corpus {cases :: [c]}
    deriving stock (Generic)

instance (FromJSON c) => FromJSON (Corpus c)

loadCorpus :: forall c. (FromJSON c) => OsPath -> IO [c]
loadCorpus file = do
    let path = toString (decodeOsPath file)
    decoded <- eitherDecodeFileStrict' @(Corpus c) path
    corpus <- requireRight (("reading " <> path <> ": ") <>) decoded
    pure corpus.cases

{- | One case of a differential test against an oracle's recorded answer:
Deslop's answer must be the oracle's. A case carrying a known gap is one
Deslop does not yet answer the way the oracle does, and must /still differ/:
closing a gap turns its own test red.
-}
corpusCase :: (HasCallStack, Eq a, Show a) => Text -> Maybe Text -> IO a -> a -> Spec
corpusCase name knownGap answer oracle =
    it (toString (name <> maybe "" (const " [known gap]") knownGap)) $ do
        actual <- answer
        case knownGap of
            Nothing -> actual `shouldBe` oracle
            Just _ -> actual `shouldNotBe` oracle

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

ap :: Text -> AbsPath
ap = absPathUnsafe . encodeOsPath

rp :: Text -> ProjectRelativePath
rp = relativePathUnsafe . encodeOsPath

mkAbsolute :: OsPath -> IO AbsPath
mkAbsolute = runEff . runFileSystemIO . fsMkAbsolute

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

prop :: String -> PropertyT IO () -> Spec
prop desc = it desc . hedgehog

-- | Extracts the value from a Maybe or fails the test beautifully.
requireJust :: (HasCallStack) => String -> Maybe a -> IO a
requireJust msg = \case
    Nothing -> expectationFailure msg >> throwIO (AssertionFailed "unreachable")
    Just x -> pure x

-- | Extracts the value from an Either or fails the test beautifully.
requireRight :: (HasCallStack) => (e -> String) -> Either e a -> IO a
requireRight formatErr = \case
    Left e -> expectationFailure (formatErr e) >> throwIO (AssertionFailed "unreachable")
    Right x -> pure x
