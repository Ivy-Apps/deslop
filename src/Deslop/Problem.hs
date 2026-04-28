module Deslop.Problem where

import Deslop.Rulebook (RuleId, RulebookId)
import System.OsPath

newtype ProblemId = ProblemId Text deriving (Show, Eq, Ord)

data Problem
    = LintProblem
        { id :: ProblemId
        , lintRule :: LintRuleId
        , location :: Location
        , severity :: Severity
        , description :: Text
        , fix :: Text
        }
    | RuleViolation
        { id :: ProblemId
        , rulebook :: RulebookId
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
