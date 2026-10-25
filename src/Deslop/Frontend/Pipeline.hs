{- | The language-agnostic driver every 'CompilerFrontend' runs under.

It owns what is the same in every language: which files are walked, that every
Problem is reported, that only Problems outside the Baseline are repaired, that
only @deslop fix@ writes and says what it modified - in path order, however the
files happened to finish - and how many files are worked on at once. A frontend supplies the stages; it never sees the Command.
-}
module Deslop.Frontend.Pipeline (runFrontend) where

import Deslop.Command (Command (..))
import Deslop.Error (DeslopError (FrontendError), Warning (..))
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (applyFindings, problem)
import Deslop.Module (Module)
import Deslop.Problem.Baseline (Baseline)
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effects.CLI (CLI, cliLog)
import Effects.CLI qualified as CLI
import Effects.Concurrency (poolSize)
import Effects.FileSystem (RoFileSystem, WrFileSystem, fsReadFile, fsWriteFile)
import Effects.ReportProblem (ReportProblem, report)
import FileSystem.Path (
    AbsPath,
    ProjectRelativePath (osPath),
    ProjectRoot (..),
    decodeOsPath,
    relativePathTo,
 )
import Git.Ignore (loadGitIgnore)
import Renderable qualified as R

{- | Lints every file of the project, repairs and writes back what @deslop fix@
may, and lowers every one of them into 'Module's: 'parse' is total, so no file
the walk found is missing from what 'lower' sees.
-}
runFrontend ::
    ( CompilerFrontend fe
    , RoFileSystem :> es
    , WrFileSystem :> es
    , CLI :> es
    , Error DeslopError :> es
    , ReportProblem :> es
    , Concurrent :> es
    ) =>
    fe ->
    Command ->
    Baseline ->
    ProjectRoot ->
    Eff es [Module]
runFrontend fe command baseline projectRoot = do
    (cfg, cfgWs) <-
        loadConfig fe projectRoot
            >>= either (throwError . FrontendError . R.render) pure
    traverse_ (\(Warning w) -> logWarning w) cfgWs
    gitIgnore <- loadGitIgnore (ecosystemIgnored fe cfg) projectRoot.path
    files <- discover fe cfg projectRoot gitIgnore
    runReader @ProjectRoot projectRoot $ do
        (writes, units) <-
            unzip <$> pooledMapConcurrentlyN poolSize (deslopFile fe cfg command baseline) files
        traverse_ logModified . sort . mapMaybe rewritten $ writes
        (ms, lowerWs) <- lower fe cfg units
        traverse_ (logWarning . R.render) lowerWs
        pure ms
  where
    logWarning :: (CLI :> es') => Text -> Eff es' ()
    logWarning = cliLog CLI.Warning . ("WARNING: " <>)

    logModified = cliLog CLI.Change . ("  modified  " <>) . decodeOsPath . (.osPath)

-- | Whether a file's bytes were written back, and where it lives if so.
data WriteBack
    = Untouched
    | Rewritten !ProjectRelativePath

rewritten :: WriteBack -> Maybe ProjectRelativePath
rewritten Untouched = Nothing
rewritten (Rewritten path) = Just path

{- | One file through parse and lint, repaired and - for @deslop fix@ - written
back. The repaired unit is what gets lowered, whatever the Command.

It does not log the write itself: files finish in whatever order the pool runs
them, and the changelog must not depend on that.
-}
deslopFile ::
    ( CompilerFrontend fe
    , RoFileSystem :> es
    , WrFileSystem :> es
    , Reader ProjectRoot :> es
    , ReportProblem :> es
    ) =>
    fe ->
    Config fe ->
    Command ->
    Baseline ->
    AbsPath ->
    Eff es (WriteBack, Unit fe)
deslopFile fe cfg command baseline src = do
    bytes <- fsReadFile src
    let unit = parse fe cfg src bytes
    findings <- lint fe cfg unit
    traverse_ (report . problem) findings
    let repaired = applyFindings baseline unit findings
    written <- case command of
        FixC -> writeBack src bytes . render fe $ repaired
        CheckC -> pure Untouched
        BaselineC -> pure Untouched
        ExploreC _ -> pure Untouched
    pure (written, repaired)

-- | Writes a repaired file, but only one whose bytes actually changed.
writeBack ::
    (WrFileSystem :> es, Reader ProjectRoot :> es) =>
    AbsPath ->
    ByteString ->
    ByteString ->
    Eff es WriteBack
writeBack src original repaired
    | repaired == original = pure Untouched
    | otherwise = do
        fsWriteFile src repaired
        projectRoot <- ask @ProjectRoot
        pure . Rewritten . relativePathTo projectRoot $ src
