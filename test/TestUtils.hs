module TestUtils (
    snapshot,
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
    emptyTsConfig,
    mkMapping,
    mkAbsolute,
    pathSafeGolden,
    requireJust,
    requireRight,
    ap,
    rp,
    baselineOf,
    mkImportNode,
    failBeatiful,
    mkUsesImportDto,
    mkForbidsImportDto,
    mkExistsModuleDto,
    rulebookDto,
    ruleDto,
) where

import Control.Exception (throwIO)
import Control.Exception.Base (AssertionFailed (..))
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop.AST (AstNode (..))
import Deslop.Baseline (Baseline (..))
import Deslop.Problem (ProblemId (..))
import Deslop.Rulebook (ExistsDto (ExistsModuleDto), ForbidsDto (..), GlobDto (GlobDto), RuleDto (..), RuleId (..), RulebookDto (..), UsesDto (UsesImportDto))
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.AI
import Effects.CLILog
import Effects.FileSystem (AbsPath (osPath), RelativePath, absPathUnsafe, encodeOsPath, encodeOsPathString, fsMkAbsolute, relativePathUnsafe, runFileSystemIO)
import Effects.Git
import Params
import Secrets (GeminiApiKey (..), Secrets (..))
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, takeExtension, (</>))
import Test.Hspec (expectationFailure)
import Test.Hspec.Golden (Golden, defaultGolden)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import TypeScript.ModuleResolver (moduleIdUnsafe)
import Types (Renderable (render))
import UI (problemsLogText)

type ModifiedFiles = [OsPath]

newtype TestLogs = TestLogs
    { problems :: Text
    }
    deriving (Show, Eq)

runCLILogTest :: (IOE :> es) => IORef (Maybe TestLogs) -> Eff (CLILog : es) a -> Eff es a
runCLILogTest ref = interpret $ \_ -> \case
    LogTitle _ -> pure ()
    LogModification _ -> pure ()
    LogFixSummary -> pure ()
    LogProblems ps ->
        liftIO $ writeIORef ref (Just . TestLogs . problemsLogText $ ps)
    LogNoProblemsFound -> pure ()
    LogBaselineSaved _ -> pure ()
    LogError _ -> pure ()

defaultParams :: OsPath -> IO Params
defaultParams projPath = do
    absProjPath <- mkAbsolute projPath
    pure
        Params
            { projectPath = absProjPath
            , command = FixC
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

defaultTsConfig :: TsConfig
defaultTsConfig =
    TsConfig
        { baseUrl = absPathUnsafe [osp|/home/repo|]
        , paths =
            [ mkMapping (Wildcard "@test/" "") [Wildcard "test/" ""]
            , mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]
            ]
        }

emptyTsConfig :: TsConfig
emptyTsConfig = TsConfig {baseUrl = absPathUnsafe [osp|/home/repo|], paths = []}

mkMapping :: Pattern -> [Pattern] -> PathMapping
mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)

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

failBeatiful :: (HasCallStack) => Text -> IO a
failBeatiful msg = expectationFailure (show msg) >> throwIO (AssertionFailed "unreachable")

ap :: Text -> AbsPath
ap = absPathUnsafe . encodeOsPath

rp :: Text -> RelativePath
rp = relativePathUnsafe . encodeOsPath

baselineOf :: [Text] -> Baseline
baselineOf = Baseline . HS.fromList . fmap ProblemId

-- | Constructs an ImportNode with a realistic raw import statement.
mkImportNode :: Text -> AstNode
mkImportNode t =
    ImportNode
        { target = moduleIdUnsafe t
        , rawStatement = "import { ... } from '" <> t <> "'"
        }

mkForbidsImportDto :: Text -> Bool -> ForbidsDto
mkForbidsImportDto p transitive = ForbidsImportDto (GlobDto p) (Just transitive)

mkUsesImportDto :: Text -> Bool -> UsesDto
mkUsesImportDto p transitive = UsesImportDto (GlobDto p) (Just transitive)

mkExistsModuleDto :: Text -> ExistsDto
mkExistsModuleDto = ExistsModuleDto . GlobDto

rulebookDto :: RulebookDto
rulebookDto =
    RulebookDto
        { id = "test-rulebook"
        , rules = []
        , name = "Test rulebook"
        , description = "Rulebook used for testing"
        }

ruleDto :: RuleDto
ruleDto =
    RuleDto
        { id = RuleId "test-rule"
        , description = "test"
        , target = GlobDto ""
        , exclude = Nothing
        , executionContext = Nothing
        , forbids = Nothing
        , uses = Nothing
        , exists = Nothing
        , fix = ""
        , example = Nothing
        }
