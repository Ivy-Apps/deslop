{-# LANGUAGE QuasiQuotes #-}

module Deslop (
    deslopFile,
    doWork,
    runDeslop,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.AST (AstModule)
import Deslop.CodeGraph (ModuleGraph, buildModuleGraph)
import Deslop.Error (DeslopError (..))
import Deslop.Problem (Problem, isAutoFixable)
import Deslop.Problem.Baseline (Baseline, applyBaseline, emptyBaseline, loadBaseline, saveBaseline)
import Deslop.Problem.Shrinker (compactProblems)
import Deslop.Rule.Book (Rulebook (..))
import Deslop.Rule.Book.Loader (loadRulebooks)
import Deslop.Rule.Enforcer (enforceRulebooks)
import Deslop.Rule.Lint.CycleDetection (noImportCycles)
import Deslop.RunReport
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, asks, runReader)
import Effects.CLI (CLI, LogStyle (..), cliLog, runCLI)
import Effects.FileSystem (
    RoFileSystem,
    WrFileSystem,
    fsReadFile,
    fsWriteFile,
    runFileSystemIO,
 )
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import FileSystem.Path (AbsPath (osPath), ProjectRoot (..), decodeOsPath, withAbsBaseUnsafe)
import Git.Ignore (loadGitIgnore)
import Params
import Renderable (Renderable (render))
import System.OsPath (osp)
import TypeScript.AST (parseAst)
import TypeScript.CST
import TypeScript.Config (TsConfig (..))
import TypeScript.Config.Loader (loadTsConfig, renderSkippedExtends, renderTsConfigLoadError)
import TypeScript.Iterator (getTsFiles)
import TypeScript.Lint.RelativeImports (noRelativeImports)
import TypeScript.Parser (TsFile (TsFile, content, path), parseTs)
import UI (divider, humanReadable, problemsFoundText, problemsLogText, summaryLine)
import Utils (pluralise)

runDeslop :: ParamsDto -> IO ()
runDeslop paramsDto =
    runEff
        . runFileSystemIO
        . runCLI
        . runConcurrent
        . runReportProblem
        $ do
            start <- liftIO getCurrentTime
            res <-
                runErrorNoCallStack @DeslopError $
                    paramsFromDto paramsDto >>= doWork
            end <- liftIO getCurrentTime
            case res of
                Left err -> failWith . humanReadable $ err
                Right report -> do
                    cliLog Plain . summaryLine report.summary $ diffUTCTime end start
                    case report.verdict of
                        Clean -> pure ()
                        ProblemsFound counts -> failWith . problemsFoundText $ counts
  where
    failWith msg = do
        cliLog Error $ "❌ Error: " <> msg
        liftIO exitFailure

doWork ::
    ( WrFileSystem :> es
    , RoFileSystem :> es
    , CLI :> es
    , IOE :> es
    , Concurrent :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    Params ->
    Eff es RunReport
doWork params = do
    logTitle params
    case params.command of
        FixC -> do
            baseline <- loadBaseline params.projectPath
            summary <- deslopProject params baseline
            ps <- compactProblems . applyBaseline baseline <$> getProblems
            logFixSummary . length . filter isAutoFixable $ ps
            pure RunReport {summary = summary, verdict = Clean}
        CheckC -> do
            baseline <- loadBaseline params.projectPath
            summary <- deslopProject params baseline
            ps <- compactProblems . applyBaseline baseline <$> getProblems
            verdict <- case ps of
                [] -> do
                    cliLog Success "✅ Success: No problems found."
                    pure Clean
                _ -> do
                    logProblems ps
                    pure . ProblemsFound $
                        ProblemCounts
                            { total = length ps
                            , autoFixable = length . filter isAutoFixable $ ps
                            }
            pure RunReport {summary = summary, verdict = verdict}
        BaselineC -> do
            summary <- deslopProject params emptyBaseline
            ps <- compactProblems <$> getProblems
            saveBaseline params.projectPath ps
            cliLog Success $
                "✅ Success: Baseline generated with "
                    <> pluralise (length ps) "problem"
                    <> "."
            pure RunReport {summary = summary, verdict = Clean}

logTitle :: (CLI :> es) => Params -> Eff es ()
logTitle params = do
    cliLog Title $
        "🚀 "
            <> commandTitle params.command
            <> " project: "
            <> decodeOsPath params.projectPath.osPath
    case params.command of
        FixC -> cliLog Plain "Changelog:"
        _ -> pure ()

commandTitle :: Command -> Text
commandTitle CheckC = "Checking"
commandTitle FixC = "Fixing"
commandTitle BaselineC = "Baselining"

logProblems :: (CLI :> es) => [Problem] -> Eff es ()
logProblems ps = do
    cliLog Error $ "Found " <> pluralise (length ps) "problem" <> ":"
    cliLog Error divider
    cliLog Error . problemsLogText $ ps
    cliLog Error divider

-- | Reports how many auto-fixable Problems @deslop fix@ resolved.
logFixSummary :: (CLI :> es) => Int -> Eff es ()
logFixSummary fixed = do
    cliLog Plain divider
    cliLog Success $ case fixed of
        0 -> "✨ The project is already clean!"
        n -> "✨ Fixed " <> pluralise n "problem" <> "!"
    cliLog Plain divider

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
    Eff es RunSummary
deslopProject params baseline = do
    rulebookRes <- loadRulebooks params.projectPath
    rulebook <- case rulebookRes of
        Right rb -> pure rb
        Left e -> throwError . RulebookError $ e
    logRulebooks params.command rulebook

    cfg <- tsConfig params.projectPath
    gitIgnore <- loadGitIgnore params.projectPath
    files <- getTsFiles gitIgnore params.projectPath
    (lintErrors, asts) <-
        fmap partitionEithers
            . runReader @TsConfig cfg
            . runReader @Params params
            . runReader @Baseline baseline
            $ pooledMapConcurrentlyN 32 deslopFile files
    traverse_ (cliLog Error . ("❌ Error: " <>) . T.pack) lintErrors
    when
        (params.command /= FixC)
        $ do
            let mg = buildModuleGraph asts
            runReader @ProjectRoot (ProjectRoot cfg.pathsBase)
                . runReader @ModuleGraph mg
                $ do
                    noImportCycles
                    runReader @[Rulebook] rulebook
                        . traverse_ enforceRulebooks
                        $ asts
    pure $
        summaryOf
            params.command
            (ModuleCount . length $ asts)
            (enforcedRules rulebook)

{- | What the command covered. @fix@ enforces no Rulebook Rules, so it reports
only the modules it went through.
-}
summaryOf :: Command -> ModuleCount -> RuleCount -> RunSummary
summaryOf CheckC ms rs = Checked ms rs
summaryOf BaselineC ms rs = Baselined ms rs
summaryOf FixC ms _ = Scanned ms

{- | Every Rule the run enforced: the Rulebooks' own, plus the built-in ones
that hold with no Rulebook at all.
-}
enforcedRules :: [Rulebook] -> RuleCount
enforcedRules rulebooks = RuleCount $ rulebookRules + buildInRulesCount
  where
    RuleCount rulebookRules = countRules rulebooks

-- | No relative imports and no import cycles.
buildInRulesCount :: Int
buildInRulesCount = 2

-- | What the Rulebooks themselves define, built-in Rules aside.
countRules :: [Rulebook] -> RuleCount
countRules = RuleCount . sum . fmap (length . (.rules))

{- | Reports what the Rulebooks contribute. Silent for @fix@, which never
enforces Rulebook Rules.
-}
logRulebooks :: (CLI :> es) => Command -> [Rulebook] -> Eff es ()
logRulebooks FixC _ = pure ()
logRulebooks _ rulebooks = do
    case rulebooks of
        [] -> pure ()
        _ -> cliLog Plain summary
    case totalRules of
        0 -> cliLog Warning noRulesWarning
        _ -> pure ()
  where
    RuleCount totalRules = countRules rulebooks
    summary =
        "📚 Loaded "
            <> pluralise (length rulebooks) "rulebook"
            <> ", "
            <> pluralise totalRules "rule"
    noRulesWarning =
        "WARNING: No architecture rules loaded. Deslop is only running its built-in checks.\n"
            <> "Define your own rules in deslop/rules/*.yaml - see https://deslop.dev"

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
        cliLog Change $ "  modified  " <> decodeOsPath src.osPath
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
    deslop = foldr (>=>) pure [noRelativeImports]

{- | The project's effective TypeScript configuration, @extends@ chain and all.

An @extends@ Deslop declines to follow is a warning rather than a failure: the
project is not broken, we simply cannot see what that package declares.
-}
tsConfig ::
    ( RoFileSystem :> es
    , Error DeslopError :> es
    , CLI :> es
    ) =>
    AbsPath ->
    Eff es TsConfig
tsConfig projPath = do
    res <- loadTsConfig $ withAbsBaseUnsafe projPath [osp|tsconfig.json|]
    case res of
        Left err -> throwError . TsConfigError . renderTsConfigLoadError $ err
        Right (cfg, skipped) -> cfg <$ traverse_ logSkipped skipped
  where
    logSkipped = cliLog Warning . ("WARNING: " <>) . renderSkippedExtends
