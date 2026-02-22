module Effects.ReportProblem (
    Location (..),
    ProblemId (..),
    Severity (..),
    Problem (..),
    ReportProblem,
    report,
    getProblems,
    runReportProblem,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Function ((&))
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic

data Location = Location
    { file :: FilePath
    , code :: Text
    }
newtype ProblemId = ProblemId Text

data Severity = Error

data Problem = Problem
    { id :: ProblemId
    , location :: Location
    , severity :: Severity
    , description :: Text
    , fix :: Text
    }

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
    problemsVar <- liftIO $ newTVarIO []
    action
        & interpret
            ( \_ -> \case
                Report p -> liftIO . atomically $ modifyTVar' problemsVar (p :)
                GetProblems -> liftIO . readTVarIO $ problemsVar
            )
