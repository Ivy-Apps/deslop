module Deslop.Problem (
    Problem (..),
    ProblemId (..),
    problemId,
    Location (..),
    LintRuleId (..),
    Severity (..),
) where

import Deslop.Rulebook (RuleId (RuleId), RulebookId (RulebookId))
import Effects.FileSystem (RelativePath)
import TypeScript.ModuleResolver (ModuleId (..))

newtype ProblemId = ProblemId Text deriving (Show, Eq, Ord)

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
        , targetModule :: ModuleId
        , description :: Text
        , fix :: Text
        }
    deriving stock (Eq, Show, Ord)

data Location = Location
    { file :: RelativePath
    , code :: Text
    }
    deriving stock (Eq, Show, Ord)

newtype LintRuleId = LintRuleId Text
    deriving stock (Eq, Show, Ord)

data Severity = Error
    deriving stock (Eq, Show, Ord)

problemId :: Problem -> ProblemId
problemId
    LintProblem
        { lintRule = LintRuleId rId
        } = ProblemId $ rId
problemId
    p@RuleViolation
        { rulebook = RulebookId rbId
        , rule = RuleId rId
        } =
        let
            mId = p.targetModule.text
         in
            ProblemId $ rbId <> "#" <> rId <> "#" <> mId
