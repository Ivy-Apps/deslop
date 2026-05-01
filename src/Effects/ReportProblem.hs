module Effects.ReportProblem (
    ReportProblem,
    report,
    getProblems,
    runReportProblem,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Deslop.Problem (Problem)
import Effectful
import Effectful.Dispatch.Dynamic

data ReportProblem :: Effect where
    Report :: Problem -> ReportProblem m ()
    GetProblems :: ReportProblem m [Problem]

type instance DispatchOf ReportProblem = 'Dynamic

report :: (ReportProblem :> es) => Problem -> Eff es ()
report = send . Report

getProblems :: (ReportProblem :> es) => Eff es [Problem]
getProblems = send GetProblems

runReportProblem :: (IOE :> es) => Eff (ReportProblem : es) a -> Eff es a
runReportProblem action = do
    problemsVar <- liftIO $ newTVarIO ([] :: [Problem])
    action
        & interpret
            ( \_ -> \case
                Report p -> liftIO . atomically . modifyTVar' problemsVar $ (p :)
                GetProblems -> liftIO (sort <$> readTVarIO problemsVar)
            )
