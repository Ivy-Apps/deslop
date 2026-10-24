{- | Runs the driver over a Lines project held in memory, and collects
everything a run leaves behind.
-}
module Fixtures.Deslop.Frontend.Pipeline (
    Outcome (..),
    runLines,
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

data Outcome = Outcome
    { modules :: [Module]
    , problems :: [Problem]
    , files :: Map AbsPath ByteString
    , written :: [AbsPath]
    , transcript :: [(LogStyle, Text)]
    }

runLines :: Command -> Baseline -> [(AbsPath, ByteString)] -> IO Outcome
runLines command baseline initial = do
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
    modules <- either (fail . show) pure result
    files <- readIORef fs.files
    written <- readIORef fs.written
    logs <- readIORef logsRef
    pure
        Outcome
            { modules
            , problems
            , files
            , written
            , transcript = logs.transcript
            }
