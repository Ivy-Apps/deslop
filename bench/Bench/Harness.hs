{- | Runs 'doWork' the way the benchmark needs it: silently, and without
touching the fixtures.

The effect stack mirrors @E2E.ProjectGoldenSpec@'s, so the benchmark measures
the same composition the golden tests verify.
-}
module Bench.Harness (
    caseParams,
    runCase,
) where

import Bench.Fixtures (Case (..), fixturePath)
import Deslop (doWork)
import Effectful (Eff, runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.CLI (CLI (..))
import Effects.FileSystem (WrFileSystem (..), fsMkAbsolute, runRoFileSystemIO)
import Effects.ReportProblem (runReportProblem)
import Params (Params (..))
import Types (DeslopError)

{- | Resolves a Case to the Params that run it.

Kept out of 'runCase' on purpose: path canonicalisation is a syscall, and it
belongs to setting the benchmark up rather than to the work being measured.
-}
caseParams :: Case -> IO Params
caseParams c = do
    projectPath <- runEff . runRoFileSystemIO . fsMkAbsolute . fixturePath $ c.fixture
    pure Params {projectPath = projectPath, command = c.command}

{- | Runs one case to completion, discarding everything it produces.

The 'DeslopError' is discarded rather than inspected: @check@ against a fixture
with problems legitimately ends in @Left CheckModeFoundProblems@, and that is
still a full run's worth of work.
-}
runCase :: Params -> IO ()
runCase params =
    void
        . runEff
        . runDiscardingWrFileSystem
        . runRoFileSystemIO
        . runErrorNoCallStack @DeslopError
        . runSilentCLI
        . runReportProblem
        . runConcurrent
        $ doWork params

{- | Throws every write away.

This is what keeps the benchmark from mutating the fixtures it reads in place.
The guarantee is structural rather than procedural: 'WrFileSystem' is the only
way this codebase can reach the disk, so a run wired through this interpreter
cannot change a fixture no matter what 'doWork' does.

Do not replace this with a copy into a temp directory. Criterion fits a
regression across many iterations at varying iteration counts, so every
iteration has to do identical work. Real writes break that - the first @fix@
iteration rewrites its files and every later one finds nothing left to fix, so
the fit spans two different workloads. Re-copying per iteration does not rescue
it either: criterion cannot subtract environment setup from the measurement, and
copying a 240-file project dwarfs the work being measured.
-}
runDiscardingWrFileSystem :: Eff (WrFileSystem : es) a -> Eff es a
runDiscardingWrFileSystem = interpret $ \_ -> \case
    WriteFile _path _content -> pure ()
    MkDirP _path -> pure ()

-- | Silences the log. Deslop's output is not what is being measured.
runSilentCLI :: Eff (CLI : es) a -> Eff es a
runSilentCLI = interpret $ \_ -> \case
    Log _style _msg -> pure ()
    ReadLine -> error "runSilentCLI: doWork must never read from stdin"
