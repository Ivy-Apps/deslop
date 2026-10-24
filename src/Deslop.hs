{-# LANGUAGE QuasiQuotes #-}

module Deslop (
    doWork,
    runDeslop,
) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Deslop.CodeGraph (ModuleGraph, buildModuleGraph)
import Deslop.Diagram (diagramOf)
import Deslop.Diagram.Html (explorerPage)
import Deslop.Error (DeslopError (..))
import Deslop.Frontend (CompilerFrontend (..), SomeFrontend (..))
import Deslop.Frontend.Pipeline (runFrontend)
import Deslop.Module (Module)
import Deslop.Problem (LintRuleId, Problem, isAutoFixable)
import Deslop.Problem.Baseline (Baseline, applyBaseline, loadBaseline, saveBaseline)
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
    fsMkDirP,
    fsWriteFile,
    runFileSystemIO,
 )
import Effects.ReportProblem (ReportProblem, getProblems, runReportProblem)
import FileSystem.Path (
    AbsPath (osPath),
    ProjectRoot (..),
    decodeOsPath,
    portablePath,
    relativePathTo,
    withAbsBaseUnsafe,
 )
import Frontend (detectFrontend, lookedFor)
import Params (Command (..), Params (..), ParamsDto (..), paramsFromDto)
import System.OsPath (osp, takeFileName)
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
    mfe <- detectFrontend projectRoot
    logTitle params mfe
    baseline <- baselineForCmd params
    rulebooks <- rulebooksFor params.command projectRoot
    logRulebooks params.command rulebooks
    SomeFrontend fe <- maybe (throwError $ NoFrontendDetected projectRoot lookedFor) pure mfe
    (summary, modules) <- deslopProject params fe rulebooks baseline
    case params.command of
        DiagramC -> do
            drawDiagram projectRoot modules
            pure RunReport {summary = summary, verdict = Clean}
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
                _anyProblemsFound -> do
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

baselineForCmd :: (RoFileSystem :> es) => Params -> Eff es Baseline
baselineForCmd (Params BaselineC _) = pure mempty
baselineForCmd (Params DiagramC _) = pure mempty
baselineForCmd (Params _ projectPath) = loadBaseline projectPath

{- | The Rulebooks a command enforces. A diagram draws structure and judges
none of it, so it does not read them - nor fail on a broken one.
-}
rulebooksFor :: (RoFileSystem :> es, Error DeslopError :> es) => Command -> ProjectRoot -> Eff es [Rulebook]
rulebooksFor DiagramC _ = pure []
rulebooksFor _ projectRoot =
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
commandTitle DiagramC = "Diagramming"

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
    Eff es (RunSummary, [Module])
deslopProject params fe rulebook baseline = do
    asts <- runFrontend fe params.command baseline projectRoot
    when
        (enforcesRules params.command)
        $ do
            let mg = buildModuleGraph asts
            runReader @ProjectRoot projectRoot
                . runReader @ModuleGraph mg
                $ do
                    noImportCycles
                    runReader @[Rulebook] rulebook
                        . traverse_ enforceRulebooks
                        $ asts
    pure
        ( summaryOf
            params.command
            (ModuleCount . length $ asts)
            (enforcedRules (builtInRules fe) rulebook)
        , asts
        )
  where
    projectRoot = ProjectRoot params.projectPath

-- | Whether a command judges the project: @fix@ only repairs, @diagram@ only draws.
enforcesRules :: Command -> Bool
enforcesRules CheckC = True
enforcesRules BaselineC = True
enforcesRules FixC = False
enforcesRules DiagramC = False

{- | Writes the explorer page to @deslop/diagram/index.html@, then says where
to open it.
-}
drawDiagram ::
    (WrFileSystem :> es, CLI :> es) =>
    ProjectRoot ->
    [Module] ->
    Eff es ()
drawDiagram projectRoot modules = do
    fsMkDirP dir
    fsWriteFile page . encodeUtf8 . explorerPage $ diagramOf modules
    cliLog Success $
        "✅ Success: Diagram written to " <> portablePath (relativePathTo projectRoot page)
  where
    dir = withAbsBaseUnsafe projectRoot.path [osp|deslop/diagram|]
    page = withAbsBaseUnsafe dir [osp|index.html|]

{- | What the command covered. @fix@ enforces no Rulebook Rules, so it reports
only the modules it went through.
-}
summaryOf :: Command -> ModuleCount -> RuleCount -> RunSummary
summaryOf CheckC ms rs = Checked ms rs
summaryOf BaselineC ms rs = Baselined ms rs
summaryOf FixC ms _ = Scanned ms
summaryOf DiagramC ms _ = Diagrammed ms

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

{- | Reports what the Rulebooks contribute. Silent for @fix@ and @diagram@,
which never enforce Rulebook Rules.
-}
logRulebooks :: (CLI :> es) => Command -> [Rulebook] -> Eff es ()
logRulebooks FixC _ = pure ()
logRulebooks DiagramC _ = pure ()
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
