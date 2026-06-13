{-# LANGUAGE QuasiQuotes #-}

module Deslop (
    deslopFile,
    doWork,
    runDeslop,
) where

import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.AST (AstModule, parseAst)
import Deslop.Baseline (Baseline, applyBaseline, emptyBaseline, loadBaseline, saveBaseline)
import Deslop.CodeGraph (ModuleGraph, buildModuleGraph)
import Deslop.RelativeImports (importAliases)
import Deslop.RuleEnforcer (enforceRulebooks)
import Deslop.Rulebook (Rulebook, loadRuleBook)
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, asks, runReader)
import Effects.CLI (CLI, logBaselineSaved, logError, logFixSummary, logModification, logNoProblemsFound, logProblems, logText, logTitle, runCLI)
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
import Effects.Random (Random, runRandom)
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import Effects.System (System, runSystem)
import Params
import System.OsPath (osp)
import TypeScript.CST
import TypeScript.Config (TsConfig, readTsConfig)
import TypeScript.Iterator (getTsFiles)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs)
import Types
import UI

runDeslop :: ParamsDto -> IO ()
runDeslop paramsDto = do
    start <- getCurrentTime

    res <-
        runEff
            . runFileSystemIO
            . runCLI
            . runConcurrent
            . runReportProblem
            . runErrorNoCallStack @DeslopError
            . runSystem
            . runRandom
            $ do
                params <- paramsFromDto paramsDto
                doWork params

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
    , CLI :> es
    , IOE :> es
    , Concurrent :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    , System :> es
    , Random :> es
    ) =>
    Params ->
    Eff es ()
doWork params = do
    logText "[deslop] Use implies agreement to deslop.dev/terms & /privacy. Provided AS-IS & WITH ALL FAULTS (No Support/SLA)."
    logTitle params
    case params.command of
        FixC -> do
            baseline <- loadBaseline params.projectPath
            deslopProject params baseline
            logFixSummary
        CheckC -> do
            baseline <- loadBaseline params.projectPath
            deslopProject params baseline
            ps <- applyBaseline baseline <$> getProblems
            if null ps
                then logNoProblemsFound
                else do
                    logProblems ps
                    throwError CheckModeFoundProblems
        BaselineC -> do
            deslopProject params emptyBaseline
            ps <- getProblems
            saveBaseline params.projectPath ps
            logBaselineSaved (length ps)

deslopProject ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , Error DeslopError :> es
    , CLI :> es
    , ReportProblem :> es
    , Concurrent :> es
    ) =>
    Params ->
    Baseline ->
    Eff es ()
deslopProject params baseline = do
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
            . runReader @Baseline baseline
            $ pooledMapConcurrentlyN 32 deslopFile files
    traverse_ logError lintErrors
    when
        (params.command /= FixC)
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
    , Reader Baseline :> es
    , CLI :> es
    , ReportProblem :> es
    ) =>
    AbsPath ->
    Eff es (Either String AstModule)
deslopFile src = do
    c <- fsReadFile src
    cstRes <- lintFile src c
    let c' = either (const c) renderProgram cstRes
    cmd <- asks @Params (.command)
    when (c /= c' && cmd == FixC) $ do
        fsWriteFile src c'
        logModification src
    traverse parseAst cstRes
  where
    renderProgram = TE.encodeUtf8 . render . (.cst)

lintFile ::
    ( Reader TsConfig :> es
    , Reader Baseline :> es
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
