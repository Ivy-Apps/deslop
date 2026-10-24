{- | Runs the driver over a Lines project held in memory, and collects
everything a run leaves behind.
-}
module Fixtures.Deslop.Frontend.Pipeline (
    Outcome (..),
    runLines,
    runLinesExpectingFailure,
) where

import Deslop.Command (Command)
import Deslop.Error (DeslopError)
import Deslop.Frontend.Pipeline (runFrontend)
import Deslop.Module (Module)
import Deslop.Problem (Problem)
import Deslop.Problem.Baseline (Baseline)
import Doubles.CLI (MockCLI (..), TestLogs (..), defaultMockCLI, runMockCLI)
import Doubles.FileSystem (InMemoryFileSystem (..), newInMemoryFileSystem, runInMemoryFileSystem)
import Doubles.Frontend (LinesFrontend (..))
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.CLI (LogStyle)
import Effects.ReportProblem (getProblems, runReportProblem)
import FileSystem.Path (AbsPath)
import Fixtures.FileSystem.Path (projectRoot)

data Outcome a = Outcome
    { result :: a
    -- ^ The modules a run lowered, or why it failed.
    , problems :: [Problem]
    , files :: Map AbsPath ByteString
    , written :: [AbsPath]
    , transcript :: [(LogStyle, Text)]
    }

-- | A run the spec relies on completing; the test fails when it does not.
runLines :: Command -> Baseline -> [(AbsPath, ByteString)] -> IO (Outcome [Module])
runLines command baseline initial =
    runLinesOver command baseline initial >>= traverse (either (fail . show) pure)

-- | A run the spec expects to fail; the test fails when it does not.
runLinesExpectingFailure :: Command -> [(AbsPath, ByteString)] -> IO (Outcome DeslopError)
runLinesExpectingFailure command initial =
    runLinesOver command mempty initial >>= traverse (either pure (const $ fail "expected the run to fail"))

instance Functor Outcome where
    fmap f o = o {result = f o.result}

instance Foldable Outcome where
    foldMap f o = f o.result

instance Traversable Outcome where
    traverse f o = (\r -> o {result = r}) <$> f o.result

runLinesOver :: Command -> Baseline -> [(AbsPath, ByteString)] -> IO (Outcome (Either DeslopError [Module]))
runLinesOver command baseline initial = do
    fs <- newInMemoryFileSystem initial
    logsRef <- newIORef (TestLogs [])
    (result, problems) <-
        runEff
            . runInMemoryFileSystem fs
            . runMockCLI defaultMockCLI {logsRef = Just logsRef}
            . runConcurrent
            . runReportProblem
            $ (,)
                <$> runErrorNoCallStack @DeslopError (runFrontend LinesFrontend command baseline projectRoot)
                <*> getProblems
    files <- readIORef fs.files
    written <- readIORef fs.written
    logs <- readIORef logsRef
    pure
        Outcome
            { result
            , problems
            , files
            , written
            , transcript = logs.transcript
            }
