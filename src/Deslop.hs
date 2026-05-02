{-# LANGUAGE QuasiQuotes #-}

module Deslop (
    deslopFile,
    doWork,
    runDeslop,
) where

import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.AST (AstModule, parseAst)
import Deslop.Baseline (applyBaseline, loadBaseline)
import Deslop.CodeGraph (ModuleGraph, buildModuleGraph)
import Deslop.RelativeImports (importAliases)
import Deslop.RuleEnforcer (enforceRulebooks)
import Deslop.Rulebook (Rulebook, loadRuleBook)
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
    fsFileExists,
    fsReadFile,
    fsWriteFile,
    runFileSystemIO,
    withAbsBaseUnsafe,
 )
import Effects.Git
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import Params
import Secrets (Secrets (..), defaultSecrets, readSecrets)
import System.OsPath (osp)
import TypeScript.CST
import TypeScript.Config (TsConfig, readTsConfig)
import TypeScript.Iterator (getTsFiles)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs)
import Types
import UI

runDeslop :: ParamsDto -> IO ()
runDeslop paramsDto = do
    secretsRes <- runEff . runFileSystemIO $ readSecrets
    case secretsRes of
        Right secrets -> do
            run secrets
        Left _ -> do
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
                $ do
                    params <- paramsFromDto paramsDto
                    doWork params secrets

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
    logTitle params
    unless params.checkMode (liftIO . putStrLn $ "Changelog:")
    baseline <- loadBaseline params.projectPath
    deslopProject params
    bool fixResult (checkModeResult baseline) params.checkMode
  where
    checkModeResult baseline = do
        ps <- applyBaseline baseline <$> getProblems
        if null ps
            then
                logNoProblemsFound
            else do
                logProblems ps
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
    rulebookRes <- loadRuleBook params.projectPath
    rulebook <- case rulebookRes of
        Right rb -> pure rb
        Left e -> throwError . RulebookErorr $ e

    cfg <- tsConfig params.projectPath
    files <- getTsFiles params.projectPath
    (lintErrors, asts) <-
        fmap partitionEithers
            . runReader @TsConfig cfg
            . runReader @Params params
            $ pooledMapConcurrentlyN 32 deslopFile files
    traverse_ logError lintErrors
    when
        params.checkMode
        ( do
            let mg = buildModuleGraph asts
            runReader @[Rulebook] rulebook
                . runReader @ModuleGraph mg
                . traverse_ enforceRulebooks
                $ asts
        )

deslopFile ::
    ( RoFileSystem :> es
    , WrFileSystem :> es
    , Reader TsConfig :> es
    , Reader Params :> es
    , CLILog :> es
    , ReportProblem :> es
    ) =>
    AbsPath ->
    Eff es (Either String AstModule)
deslopFile src = do
    c <- fsReadFile src
    cstRes <- lintFile src c
    let c' = either (const c) renderProgram cstRes
    checkMode <- asks @Params (.checkMode)
    when (c /= c' && not checkMode) $ do
        fsWriteFile src c'
        logModification src
    traverse parseAst cstRes
  where
    renderProgram = TE.encodeUtf8 . render . (.cst)

lintFile ::
    ( Reader TsConfig :> es
    , ReportProblem :> es
    , RoFileSystem :> es
    ) =>
    AbsPath ->
    ByteString ->
    Eff es (Either String TsProgram)
lintFile p c =
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
tsConfig projPath = loadConfig (withAbsBaseUnsafe projPath [osp|tsconfig.json|])
  where
    loadConfig fp = fsFileExists fp >>= bool (handleMissing fp) (handleFound fp)
    handleFound fp = readTsConfig fp >>= either handleInvalid pure

    handleMissing = throwError . TsConfigNotFoundError . (.osPath)
    handleInvalid = throwError . TsConfigParseError
