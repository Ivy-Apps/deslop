module TestUtils (
    snapshot,
    runFileSystemTest,
    runCLILogTest,
    runGitTest,
    runAITest,
    runAIAlwaysFail,
    defaultParams,
    projectFixturePath,
    copyDir,
    listFixtures,
    fixturesBasePath,
) where

import Control.Monad (forM, forM_)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.AI
import Effects.CLILog
import Effects.FileSystem (RoFileSystem (..), WrFileSystem (..))
import Effects.Git
import System.Directory (copyFile, doesDirectoryExist, listDirectory)
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath (takeExtension, (</>))
import Translations.Translator
import Types

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
        , modified = False
        }

runGitTest :: ModifiedFiles -> Eff (Git : es) a -> Eff es a
runGitTest ms = interpret $ \_ -> \case
    ModifiedFiles -> pure ms

runAITest :: Eff (AI : es) a -> Eff es a
runAITest = interpret $ \_ -> \case
    PromptLLM _ p -> pure . bimap GenericError asAIResponse . parseTranslateResponse $ p
  where
    asAIResponse :: [(Text, Text)] -> Text
    asAIResponse ts =
        "```json\n"
            <> buildJson (upperCaseValues ts)
            <> "\n```"

    buildJson = TL.toStrict . TLE.decodeUtf8 . encodePretty . M.fromList
    upperCaseValues :: [(Text, Text)] -> [(Text, Text)] 
    upperCaseValues = fmap (second T.toUpper) 


runAIAlwaysFail :: Eff (AI : es) a -> Eff es a
runAIAlwaysFail = interpret $ \_ -> \case
    PromptLLM _ _ -> pure . Left . GenericError $ "Mocked to fail"

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

listFixtures :: FilePath -> String -> IO [FilePath]
listFixtures dir ext = do
    files <- listDirectory dir
    return $ filter (\f -> takeExtension f == ext) files

fixturesBasePath :: FilePath
fixturesBasePath = "test/fixtures"
