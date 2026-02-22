module Effects.ReportProblem (
    Location (..),
    RuleId (..),
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
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Fmt (Buildable (..), (+|), (|+))

data Location = Location
    { file :: FilePath
    , code :: Text
    }
    deriving stock (Eq, Show)

newtype RuleId = RuleId Text
    deriving stock (Eq, Show)

data Severity = Error
    deriving stock (Eq, Show)

data Problem = LintProblem
    { rule :: RuleId
    , location :: Location
    , severity :: Severity
    , description :: Text
    , fix :: Text
    }
    deriving stock (Eq, Show)

instance Buildable Problem where
    build p =
        let (RuleId ruleId) = p.rule
         in problemHeader ruleId <> description <> code <> fix
      where
        problemHeader ruleId = "# " +| p.location.file |+ ": " +| ruleId |+ "\n"
        code = "```ts\n" +| T.strip p.location.code |+ "\n```\n"
        description = "" +| p.description |+ "\n"
        fix = "FIX: " +| T.strip p.fix |+ ""

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
