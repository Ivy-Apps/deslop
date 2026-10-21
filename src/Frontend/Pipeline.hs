module Frontend.Pipeline (runFrontend) where

import Deslop.Error (DeslopError)
import Deslop.Module (Module)
import Deslop.Problem.Baseline (Baseline)
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effects.CLI (CLI)
import Effects.FileSystem (RoFileSystem, WrFileSystem)
import Effects.ReportProblem (ReportProblem)
import FileSystem.Path (ProjectRoot (..))
import Frontend.Class (CompilerFrontend)
import Git.Ignore (GitIgnore)

runFrontend ::
    ( CompilerFrontend fe
    , RoFileSystem :> es
    , WrFileSystem :> es
    , Reader ProjectRoot :> es
    , Reader Baseline :> es
    , Reader GitIgnore :> es
    , CLI :> es
    , Error DeslopError :> es
    , ReportProblem :> es
    , Concurrent :> es
    ) =>
    fe -> Eff es [Module]
runFrontend = todo
