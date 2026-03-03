module Deslop (
    deslopFile,
    deslopProject,
    doWork,
    runDeslop,
    translateProject,
) where

import Control.Monad (unless, when, (>=>))
import Data.Bool
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List (intersect)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.AST (AstModule, parseAst)
import Deslop.RelativeImports (importAliases)
import Effectful (Eff, IOE, liftIO, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, asks, runReader)
import Effects.AI
import Effects.CLILog
import Effects.FileSystem (
    RoFileSystem,
    WrFileSystem,
    directoryExists,
    fileExists,
    isDirectory,
    listDirectory,
    readFileBS,
    runFileSystemIO,
    writeFileBS,
 )
import Effects.Git
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import Fmt (fmt, (+|), (|+))
import Params
import Secrets (Secrets (..), defaultSecrets, readSecrets)
import System.Exit (exitFailure)
import System.FilePath
import Translations.Manager
import Translations.Parser
import TypeScript.CST
import TypeScript.Config (TsConfig, parseTsConfig)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs)
import Types
import UI

runDeslop :: Params -> IO ()
runDeslop params = do
    secretsRes <- runEff . runFileSystemIO $ readSecrets
    case secretsRes of
        Right secrets -> do
            when
                (isNothing secrets.geminiApiKey)
                (printWarning "AI features disabled because Gemini API key is not provided in ~/.deslop/secrets.json")
            run secrets
        Left err -> do
            printWarning $ "AI features disabled because - " <> show err
            run defaultSecrets
  where
    run secrets = do
        start <- getCurrentTime

        res <-
            runEff
                . runFileSystemIO
                . runCLILog
                . runGit
                . runAI secrets
                . runConcurrent
                . runReportProblem
                . runErrorNoCallStack @DeslopError
                $ doWork params secrets

        end <- liftIO getCurrentTime
        let diff = diffUTCTime end start
        let seconds = realToFrac diff :: Double
        case res of
            Left err -> do
                liftIO $ printErr (humanReadable err)
                exitFailure
            Right _ -> printTime seconds

doWork ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , CLILog :> es
    , IOE :> es
    , AI :> es
    , Concurrent :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    Params ->
    Secrets ->
    Eff es ()
doWork params _ = do
    liftIO . printTitle $ "🚀 Deslopping project: " <> T.pack params.projectPath
    unless params.checkMode (liftIO . putStrLn $ "Changelog:")
    deslopProject params
    bool fixResult checkModeResult params.checkMode
  where
    checkModeResult = do
        ps <- getProblems
        if null ps
            then
                liftIO $ printSuccess "No problems found."
            else do
                liftIO $ putStderrLn (fmt $ "Found " +| length ps |+ " problems:")
                liftIO printDividerStderr
                logProblems ps
                liftIO printDividerStderr
                throwError CheckModeFoundProblems

    fixResult = do
        liftIO printDivider
        unless params.checkMode logSummary
        liftIO printDivider
        doTranslations params

deslopProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , Error DeslopError :> es
    , CLILog :> es
    , ReportProblem :> es
    ) =>
    Params ->
    Eff es ()
deslopProject params = do
    let projPath = params.projectPath
    cfg <- tsConfig projPath
    files <- getTsFiles projPath
    runReader @TsConfig cfg
        . runReader @Params params
        $ if params.modifiedOnly
            then do
                mFiles <- map normalise <$> modifiedFiles
                forM_ (mFiles `intersect` (normalise <$> files)) deslopFile
            else
                forM_ files deslopFile

getTsFiles :: (RoFileSystem :> es) => FilePath -> Eff es [FilePath]
getTsFiles dir = listDirectory dir >>= fmap concat . traverse (processEntry dir)
  where
    processEntry root entry
        | entry `elem` ignored = pure []
        | otherwise = resolve $ root </> entry

    resolve path = isDirectory path >>= bool (tsOrEmpty path) (getTsFiles path)

    tsOrEmpty f = pure [f | takeExtension f `elem` [".ts", ".tsx"]]
    ignored = ["node_modules", ".git", "dist", ".next"]

deslopFile ::
    ( RoFileSystem :> es
    , WrFileSystem :> es
    , Reader TsConfig :> es
    , Reader Params :> es
    , CLILog :> es
    , ReportProblem :> es
    ) =>
    FilePath ->
    Eff es (Either String AstModule)
deslopFile src = do
    c <- readFileBS src
    cstRes <- removeSlop src c
    let c' = either (const c) renderProgram cstRes
    checkMode <- asks @Params (.checkMode)
    when (c /= c' && not checkMode) $ do
        writeFileBS src c'
        logModification src
    either (pure . Left) (fmap Right . parseAst) cstRes
  where
    renderProgram = TE.encodeUtf8 . render . (.cst)

removeSlop ::
    (Reader TsConfig :> es, ReportProblem :> es) =>
    FilePath ->
    ByteString ->
    Eff es (Either String TsProgram)
removeSlop p c =
    traverse deslop . parseTs $
        TsFile {path = p, content = TE.decodeUtf8 c}
  where
    deslop = foldr (>=>) pure [importAliases]

doTranslations ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , IOE :> es
    , AI :> es
    , CLILog :> es
    , Concurrent :> es
    , ReportProblem :> es
    ) =>
    Params -> Eff es ()
doTranslations params = do
    liftIO . putStrLn $ "Translating..."
    translateRes <- runErrorNoCallStack @TranslationsError (translateProject params)
    case translateRes of
        Left err -> liftIO . printErr . T.pack $ show err
        Right _ -> liftIO . putStrLn $ "Translations success."

translateProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , CLILog :> es
    , AI :> es
    , Concurrent :> es
    , Error TranslationsError :> es
    ) =>
    Params ->
    Eff es ()
translateProject params =
    directoryExists translationsDir
        >>= bool
            handleFileNotFound
            (readTranslations translationsDir >>= maybe handleReadError pipeline)
  where
    pipeline ts = fixTranslations ts >>= either handleTranslateErorr writeTranslations
    writeTranslations = traverse_ writeTranslation . (.extra)
    writeTranslation (Translation l t) = writeFileBS (translationFile l) (TE.encodeUtf8 $ render t)

    translationFile l = translationsDir </> (T.unpack l <> ".json")
    translationsDir = params.projectPath </> "messages"

    handleFileNotFound = throwError MessagesNotFound
    handleReadError = throwError ParseTranslationsError
    handleTranslateErorr = throwError . TranslateError

tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    ) =>
    FilePath ->
    Eff es TsConfig
tsConfig projPath = loadConfig $ projPath </> "tsconfig.json"
  where
    loadConfig fp = fileExists fp >>= bool (handleMissing fp) (handleFound fp)
    handleFound fp = readFileBS fp >>= maybe (handleInvalid fp) pure . parseTsConfig

    handleMissing = throwError . TsConfigNotFoundError
    handleInvalid = throwError . TsConfigParseError
