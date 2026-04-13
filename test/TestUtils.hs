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
    fixturesPath,
    renderGolden,
    TestLogs (..),
    testSecrets,
    defaultTsConfig,
    mkAbsolute,
    pathSafeGolden,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.AI
import Effects.CLILog
import Effects.FileSystem (AbsPath (osPath), RoFileSystem (..), WrFileSystem (..), encodeOsPathString, fsMkAbsolute, runFileSystemIO, runRoFileSystemIO)
import Effects.Git
import Params
import Secrets (GeminiApiKey (..), Secrets (..))
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, takeExtension, (</>))
import Test.Hspec.Golden (Golden, defaultGolden)
import TypeScript.Config (ImportAlias (..), TsConfigLegacy (..))
import Types (Renderable (render))
import UI (problemsLogText)

type ModifiedFiles = [OsPath]

runFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : RoFileSystem : es) a ->
    Eff es a
runFileSystemTest ref = runRoFileSystemIO . runWrFileSystemTest ref

runWrFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : es) a ->
    Eff es a
runWrFileSystemTest ref = interpret $ \_ -> \case
    WriteFile _path content -> liftIO $ writeIORef ref (Just content)

newtype TestLogs = TestLogs
    { problems :: Text
    }
    deriving (Show, Eq)

runCLILogTest :: (IOE :> es) => IORef (Maybe TestLogs) -> Eff (CLILog : es) a -> Eff es a
runCLILogTest ref = interpret $ \_ -> \case
    LogModification _ -> pure ()
    LogSummary -> pure ()
    LogProblems ps ->
        liftIO $ writeIORef ref (Just . TestLogs . problemsLogText $ ps)
    LogError _ -> pure ()

defaultParams :: OsPath -> Params
defaultParams projPath =
    Params
        { projectPath = projPath
        , checkMode = False
        }

runGitTest :: ModifiedFiles -> Eff (Git : es) a -> Eff es a
runGitTest ms = interpret $ \_ -> \case
    ModifiedFiles -> pure ms

runAITest :: Eff (AI : es) a -> Eff es a
runAITest = interpret $ \_ -> \case
    PromptLLM _ p -> pure . Right $ p

runAIAlwaysFail :: Eff (AI : es) a -> Eff es a
runAIAlwaysFail = interpret $ \_ -> \case
    PromptLLM _ _ -> pure . Left . GenericError $ "Mocked to fail"

projectFixturePath :: OsPath
projectFixturePath = [osp|test/fixtures/ts-project-1|]

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

snapshot :: OsPath -> [String] -> IO String
snapshot tmpDir filesToVerify = do
    results <- forM filesToVerify $ \relPath -> do
        raw <- SFO.readFile' (tmpDir </> encodeOsPathString relPath)
        let content = TE.decodeUtf8 raw
        let header = "\n\n\n>>> FILE: " <> T.pack relPath <> "\n"
        return $ header <> content
    pure . T.unpack . T.dropWhile (== '\n') $ T.concat results

listFixtures :: OsPath -> String -> IO [OsPath]
listFixtures dir ext = do
    files <- SDO.listDirectory dir
    let extOs = encodeOsPathString ext
    pure $ filter (\f -> takeExtension f == extOs) files

fixturesPath :: OsPath
fixturesPath = [osp|test/fixtures|]

mkAbsolute :: OsPath -> IO AbsPath
mkAbsolute = runEff . runFileSystemIO . fsMkAbsolute

renderGolden :: (Renderable r) => String -> r -> Golden String
renderGolden testCase tree = defaultGolden testCase (T.unpack . render $ tree)

pathSafeGolden :: String -> String -> IO (Golden String)
pathSafeGolden name content = do
    baseAbsPath <- T.replace "\"" "" . T.pack . show . (.osPath) <$> mkAbsolute [osp|.|]
    let cleanContent = T.replace baseAbsPath "~" (T.pack content)
    pure $ defaultGolden name (T.unpack cleanContent)

testSecrets :: Secrets
testSecrets =
    Secrets
        { geminiApiKey = Just $ GeminiApiKey "testKey"
        }

defaultTsConfig :: TsConfigLegacy
defaultTsConfig =
    TsConfigLegacy
        { paths =
            [ ImportAlias {label = "@/", path = "src/"}
            , ImportAlias {label = "@test/", path = "test/"}
            ]
        }
