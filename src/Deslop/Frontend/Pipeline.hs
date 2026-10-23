{- | The language-agnostic driver every 'CompilerFrontend' runs under.

It owns what is the same in every language: which files are walked, that every
Problem is reported, that only Problems outside the Baseline are repaired, that
only @deslop fix@ writes and says what it modified - in path order, however the
files happened to finish - and how many files are worked on at once. A frontend supplies the stages; it never sees the Command.
-}
module Deslop.Frontend.Pipeline (runFrontend) where

import Deslop.Command (Command (..))
import Deslop.Error (DeslopError)
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (applyFindings)
import Deslop.Module (Module)
import Deslop.Problem.Baseline (Baseline)
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effects.CLI (CLI, LogStyle (..), cliLog)
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
import Renderable qualified

{- | Lints every file of the project, repairs and writes back what @deslop fix@
may, and lowers what parsed into 'Module's.

A file that does not parse is logged and left out; the run goes on without it.
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
    cfg <- loadConfig fe projectRoot
    gitIgnore <- loadGitIgnore (ecosystemIgnored fe) projectRoot.path
    files <- discover fe cfg projectRoot gitIgnore
    runReader @ProjectRoot projectRoot $ do
        (writes, results) <-
            unzip <$> pooledMapConcurrentlyN 32 (deslopFile fe cfg command baseline) files
        traverse_ logModified . sort . mapMaybe rewritten $ writes
        let (parseErrors, units) = partitionEithers results
        traverse_ (cliLog Error . ("❌ Error: " <>) . Renderable.render) parseErrors
        lower fe cfg units
  where
    logModified = cliLog Change . ("  modified  " <>) . decodeOsPath . (.osPath)

-- | Whether a file's bytes were written back, and where it lives if so.
data WriteBack
    = Untouched
    | Rewritten ProjectRelativePath

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
    Eff es (WriteBack, Either (ParseError fe) (Unit fe))
deslopFile fe cfg command baseline src = do
    bytes <- fsReadFile src
    case parse fe cfg src bytes of
        Left err -> pure (Untouched, Left err)
        Right unit -> fmap Right <$> lintUnit bytes unit
  where
    lintUnit bytes unit = do
        findings <- lint fe cfg unit
        traverse_ (report . (.problem)) findings
        let repaired = applyFindings baseline unit findings
        written <- case command of
            FixC -> writeBack src bytes . render fe $ repaired
            CheckC -> pure Untouched
            BaselineC -> pure Untouched
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
