module TestUtils (
    snapshot,
    runFileSystemTest,
    runCLILogTest,
    runGitTest,
    defaultParams,
    projectFixturePath,
    copyDir,
) where

import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Deslop (Params (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.CLILog
import Effects.FileSystem (RoFileSystem (..), WrFileSystem (..))
import Effects.Git
import System.Directory (copyFile, doesDirectoryExist, listDirectory)
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath ((</>))

type ModifiedFiles = [FilePath]

runFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : RoFileSystem : es) a ->
    Eff es a
runFileSystemTest ref = runRoFileSystemTest . runWrFileSystemTest ref

runRoFileSystemTest ::
    (IOE :> es) =>
    Eff (RoFileSystem : es) a ->
    Eff es a
runRoFileSystemTest = interpret $ \_ -> \case
    ReadFile path -> liftIO $ BS.readFile path
    FileExists _path -> pure True
    ListDirectory _path -> pure []
    IsDirectory _path -> pure False

runWrFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : es) a ->
    Eff es a
runWrFileSystemTest ref = interpret $ \_ -> \case
    WriteFile _path content -> liftIO $ writeIORef ref (Just content)

runCLILogTest :: Eff (CLILog : es) a -> Eff es a
runCLILogTest = interpret $ \_ -> \case
    LogModification _ -> pure ()
    LogSummary -> pure ()

defaultParams :: FilePath -> Params
defaultParams projPath =
    Params
        { projectPath = projPath
        , imports = True
        , comments = True
        , modified = False
        }

runGitTest :: ModifiedFiles -> Eff (Git : es) a -> Eff es a
runGitTest ms = interpret $ \_ -> \case
    ModifiedFiles -> pure ms

projectFixturePath :: FilePath
projectFixturePath = "test/fixtures/ts-project-1"

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
    createDirectoryIfMissing True dst
    content <- listDirectory src
    forM_ content $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDir srcPath dstPath
            else copyFile srcPath dstPath

snapshot :: FilePath -> [FilePath] -> IO String
snapshot tmpDir filesToVerify = do
    results <- forM filesToVerify $ \relPath -> do
        content <- TIO.readFile (tmpDir </> relPath)
        let header = "\n\n\n>>> FILE: " <> T.pack relPath <> "\n"
        return $ header <> content
    pure . T.unpack . T.dropWhile (== '\n') $ T.concat results
