{- | Test plumbing: golden helpers, fixture-directory access, and the small
combinators that keep a failing assertion readable.

Deliberately domain-free. A helper that builds a 'Deslop.Module.Module' or a
'TypeScript.Config.TsConfig' belongs under @Fixtures.@ next to the module whose
type it builds; a helper that only knows about hspec, Hedgehog or the
filesystem belongs here.
-}
module TestUtils (
    -- * Golden tests
    snapshot,
    renderGolden,
    pathSafeGolden,

    -- * Fixtures on disk
    fixturesPath,
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
    AbsPath (osPath),
    ProjectRelativePath,
    absPathUnsafe,
    encodeOsPath,
    encodeOsPathString,
    relativePathUnsafe,
 )
import Hedgehog (PropertyT)
import Renderable (Renderable (render))
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, takeExtension, (</>))
import Test.Hspec (Spec, expectationFailure, it)
import Test.Hspec.Golden (Golden, defaultGolden)
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

renderGolden :: (Renderable r) => String -> r -> Golden String
renderGolden testCase tree = defaultGolden testCase (T.unpack . render $ tree)

{- | Goldens a value that holds an absolute path /by design/, with the
directory the suite runs from replaced so the snapshot is the same on every
machine.

For internal domain values only - a 'TypeScript.Config.TsConfig' carries an
absolute paths base because the resolver has to open files with it. Never for
what Deslop prints: every path a run reports is relative to the project root,
and scrubbing that output would hide an absolute one rather than fail on it,
which is exactly how one reached a committed baseline.
-}
pathSafeGolden :: String -> String -> IO (Golden String)
pathSafeGolden name content = do
    baseAbsPath <- T.replace "\"" "" . T.pack . show . (.osPath) <$> mkAbsolute [osp|.|]
    let cleanContent = T.replace baseAbsPath "~" (T.pack content)
    pure $ defaultGolden name (T.unpack cleanContent)

--------------------------------------------------------------------------------
-- Fixtures on disk
--------------------------------------------------------------------------------

fixturesPath :: OsPath
fixturesPath = [osp|fixtures|]

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
