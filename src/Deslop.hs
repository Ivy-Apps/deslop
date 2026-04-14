{-# LANGUAGE QuasiQuotes #-}

module Deslop (
    deslopFile,
    deslopProject,
    doWork,
    runDeslop,
) where

import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.AST (AstModule, parseAst)
import Deslop.RelativeImports (importAliases)
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, asks, runReader)
import Effects.AI
import Effects.CLILog
import Effects.FileSystem (
    AbsPath (osPath),
    RoFileSystem,
    WrFileSystem,
    decodeOsPath,
    encodeOsPath,
    fsFileExistsAbs,
    fsIsDirectory,
    fsListDirectory,
    fsMkAbsolute,
    fsReadFile,
    fsWriteFile,
    runFileSystemIO,
    withAbsBaseUnsafe,
 )
import Effects.Git
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import Fmt (fmt, (+|), (|+))
import Params
import Secrets (Secrets (..), defaultSecrets, readSecrets)
import System.OsPath (OsPath, osp, takeExtension, (</>))
import TypeScript.CST
import TypeScript.Config (TsConfig, readTsConfig)
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
    liftIO . printTitle $ "🚀 Deslopping project: " <> decodeOsPath params.projectPath
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

deslopProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Git :> es
    , Error DeslopError :> es
    , CLILog :> es
    , ReportProblem :> es
    , Concurrent :> es
    ) =>
    Params ->
    Eff es ()
deslopProject params = do
    projPath <- fsMkAbsolute params.projectPath
    cfg <- tsConfig projPath
    files <- getTsFiles projPath.osPath
    (errors, _asts) <-
        fmap partitionEithers
            . runReader @TsConfig cfg
            . runReader @Params params
            $ pooledMapConcurrentlyN 32 deslopFile files
    traverse_ logError errors

getTsFiles :: (RoFileSystem :> es) => OsPath -> Eff es [OsPath]
getTsFiles dir = fsListDirectory dir >>= fmap concat . traverse (processEntry dir)
  where
    processEntry root entry
        | entry `elem` ignored = pure []
        | otherwise = resolve $ root </> entry

    resolve path = fsIsDirectory path >>= bool (tsOrEmpty path) (getTsFiles path)

    tsOrEmpty f = pure [f | takeExtension f `elem` [[osp|.ts|], [osp|.tsx|]]]
    ignored = map encodeOsPath ["node_modules", ".git", "dist", ".next"]

deslopFile ::
    ( RoFileSystem :> es
    , WrFileSystem :> es
    , Reader TsConfig :> es
    , Reader Params :> es
    , CLILog :> es
    , ReportProblem :> es
    ) =>
    OsPath ->
    Eff es (Either String AstModule)
deslopFile src = do
    c <- fsReadFile src
    cstRes <- removeSlop src c
    let c' = either (const c) renderProgram cstRes
    checkMode <- asks @Params (.checkMode)
    when (c /= c' && not checkMode) $ do
        fsWriteFile src c'
        logModification src
    traverse parseAst cstRes
  where
    renderProgram = TE.encodeUtf8 . render . (.cst)

removeSlop ::
    ( Reader TsConfig :> es
    , ReportProblem :> es
    , RoFileSystem :> es
    ) =>
    OsPath ->
    ByteString ->
    Eff es (Either String TsProgram)
removeSlop p c =
    traverse deslop . parseTs $
        TsFile {path = p, content = TE.decodeUtf8 c}
  where
    deslop = foldr (>=>) pure [importAliases]

tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    ) =>
    AbsPath ->
    Eff es TsConfig
tsConfig projPath = loadConfig $ (withAbsBaseUnsafe projPath [osp|tsconfig.json|])
  where
    loadConfig fp = fsFileExistsAbs fp >>= bool (handleMissing fp) (handleFound fp)
    handleFound fp = readTsConfig fp >>= either handleInvalid pure

    handleMissing = throwError . TsConfigNotFoundError . (.osPath)
    handleInvalid = throwError . TsConfigParseError
