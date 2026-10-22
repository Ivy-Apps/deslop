module Deslop (
    doWork,
    runDeslop,
) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.CodeGraph (ModuleGraph, buildModuleGraph)
import Deslop.Error (DeslopError (..))
import Deslop.Frontend (CompilerFrontend (..), SomeFrontend (..))
import Deslop.Frontend.Pipeline (runFrontend)
import Deslop.Problem (LintRuleId, Problem, isAutoFixable)
import Deslop.Problem.Baseline (Baseline, applyBaseline, emptyBaseline, loadBaseline, saveBaseline)
import Deslop.Problem.Formatter (CodeFence)
import Deslop.Problem.Shrinker (compactProblems)
import Deslop.Rule.Book (Rulebook (..))
import Deslop.Rule.Book.Loader (loadRulebooks)
import Deslop.Rule.Enforcer (enforceRulebooks)
import Deslop.Rule.Lint.CycleDetection (noImportCycles, noImportCyclesRule)
import Deslop.RunReport
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (runReader)
import Effects.CLI (CLI, LogStyle (..), cliLog, runCLI)
import Effects.FileSystem (
    RoFileSystem,
    WrFileSystem,
    runFileSystemIO,
 )
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import FileSystem.Path (
    AbsPath (osPath),
    ProjectRoot (..),
    decodeOsPath,
 )
import Frontend (detectFrontend, lookedFor)
import Params
import System.OsPath (takeFileName)
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
    detected <- detectFrontend projectRoot
    logTitle params detected
    baseline <- baselineFor params
    rulebooks <- rulebooksOf projectRoot
    logRulebooks params.command rulebooks
    -- Only now, so that a project whose Rulebooks are broken hears about them
    -- first, as it did before any language had to be detected.
    SomeFrontend fe <- maybe (throwError $ NoFrontendDetected projectRoot lookedFor) pure detected
    summary <- deslopProject params fe rulebooks baseline
    case params.command of
        FixC -> do
            ps <- compactProblems . applyBaseline baseline <$> getProblems
            logFixSummary . length . filter isAutoFixable $ ps
            pure RunReport {summary = summary, verdict = Clean}
        CheckC -> do
            ps <- compactProblems . applyBaseline baseline <$> getProblems
            verdict <- case ps of
                [] -> do
                    cliLog Success "✅ Success: No problems found."
                    pure Clean
                _ -> do
                    logProblems (codeFence fe) ps
                    pure . ProblemsFound $
                        ProblemCounts
                            { total = length ps
                            , autoFixable = length . filter isAutoFixable $ ps
                            }
            pure RunReport {summary = summary, verdict = verdict}
        BaselineC -> do
            ps <- compactProblems <$> getProblems
            saveBaseline params.projectPath ps
            cliLog Success $
                "✅ Success: Baseline generated with "
                    <> pluralise (length ps) "problem"
                    <> "."
            pure RunReport {summary = summary, verdict = Clean}
  where
    projectRoot = ProjectRoot params.projectPath

-- | What has been accepted so far. Baselining starts over, accepting nothing.
baselineFor :: (RoFileSystem :> es) => Params -> Eff es Baseline
baselineFor params = case params.command of
    BaselineC -> pure emptyBaseline
    FixC -> loadBaseline params.projectPath
    CheckC -> loadBaseline params.projectPath

rulebooksOf :: (RoFileSystem :> es, Error DeslopError :> es) => ProjectRoot -> Eff es [Rulebook]
rulebooksOf projectRoot =
    loadRulebooks projectRoot >>= either (throwError . RulebookError) pure

logTitle :: (CLI :> es) => Params -> Maybe SomeFrontend -> Eff es ()
logTitle params detected = do
    cliLog Title $
        "🚀 "
            <> commandTitle params.command
            <> " project: "
            <> decodeOsPath (takeFileName params.projectPath.osPath)
    for_ detected $ \(SomeFrontend fe) ->
        cliLog Plain $ label fe <> " project detected."
    case params.command of
        FixC -> cliLog Plain "Changelog:"
        _ -> pure ()

commandTitle :: Command -> Text
commandTitle CheckC = "Checking"
commandTitle FixC = "Fixing"
commandTitle BaselineC = "Baselining"

logProblems :: (CLI :> es) => CodeFence -> [Problem] -> Eff es ()
logProblems fence ps = do
    cliLog Error $ "Found " <> pluralise (length ps) "problem" <> ":"
    cliLog Error divider
    cliLog Error . problemsLogText fence $ ps
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
    ( CompilerFrontend fe
    , WrFileSystem :> es
    , RoFileSystem :> es
    , Error DeslopError :> es
    , CLI :> es
    , ReportProblem :> es
    , Concurrent :> es
    ) =>
    Params ->
    fe ->
    [Rulebook] ->
    Baseline ->
    Eff es RunSummary
deslopProject params fe rulebook baseline = do
    asts <- runFrontend fe params.command baseline projectRoot
    when
        (params.command /= FixC)
        $ do
            let mg = buildModuleGraph asts
            runReader @ProjectRoot projectRoot
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
            (enforcedRules (builtInRules fe) rulebook)
  where
    projectRoot = ProjectRoot params.projectPath

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
enforcedRules :: [LintRuleId] -> [Rulebook] -> RuleCount
enforcedRules frontendRules rulebooks = RuleCount $ rulebookRules + length builtIn
  where
    RuleCount rulebookRules = countRules rulebooks
    -- The language's own lint rules, and the graph's: no import cycles.
    builtIn = frontendRules <> [noImportCyclesRule]

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
