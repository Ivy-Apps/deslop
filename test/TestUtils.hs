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
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effects.FileSystem (fsMkAbsolute, runFileSystemIO)
import FileSystem.Path (
    AbsPath,
    ProjectRelativePath,
    absPathUnsafe,
    encodeOsPath,
    encodeOsPathString,
    relativePathUnsafe,
 )
import Hedgehog (PropertyT)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, takeExtension, (</>))
import Test.Hspec (Spec, expectationFailure, it)
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
