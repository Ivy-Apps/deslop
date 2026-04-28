module Effects.ReportProblem (
    Location (..),
    LintRuleId (..),
    Severity (..),
    Problem (..),
    ReportProblem,
    report,
    getProblems,
    runReportProblem,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Text qualified as T
import Deslop.Rulebook (RuleId, RulebookId (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.FileSystem (decodeOsPath)
import Fmt (Buildable (..), (+|), (|+))
import System.OsPath (OsPath)

data Problem
    = LintProblem
        { lintRule :: LintRuleId
        , location :: Location
        , severity :: Severity
        , description :: Text
        , fix :: Text
        }
    | RuleViolation
        { rulebook :: RulebookId
        , rule :: RuleId
        , description :: Text
        }
    deriving stock (Eq, Show, Ord)

data Location = Location
    { file :: OsPath
    , code :: Text
    }
    deriving stock (Eq, Show, Ord)

newtype LintRuleId = LintRuleId Text
    deriving stock (Eq, Show, Ord)

data Severity = Error
    deriving stock (Eq, Show, Ord)

instance Buildable Problem where
    build p@(LintProblem (LintRuleId ruleId) _ _ _ _) =
        problemHeader <> description <> code <> fixText
      where
        problemHeader =
            "# " +| decodeOsPath p.location.file |+ ": " +| ruleId |+ "\n"
        code = "```ts\n" +| T.strip p.location.code |+ "\n```\n"
        description = "" +| p.description |+ "\n"
        fixText = "FIX: " +| T.strip p.fix |+ ""
    build p@(RuleViolation _ _ _) = show p -- TODO: implement

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
