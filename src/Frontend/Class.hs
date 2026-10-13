module Frontend.Class (CompilerFrontend (..)) where

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
import Git.Ignore (GitIgnore)
import Params (Command)

class CompilerFrontend fe where
    detect :: (RoFileSystem :> es) => ProjectRoot -> Eff es (Maybe fe)
    label :: fe -> Text
    pipeline ::
        ( RoFileSystem :> es
        , WrFileSystem :> es
        , Reader ProjectRoot :> es
        , Reader Baseline :> es
        , Reader GitIgnore :> es
        , CLI :> es
        , Error DeslopError :> es
        , ReportProblem :> es
        , Concurrent :> es
        ) =>
        fe ->
        Command ->
        Eff es ([Either String Module])
