module Deslop.Problem (
    Problem (..),
    ProblemId (..),
    problemId,
    Location (..),
    LintRuleId (..),
) where

import Deslop.Rulebook (RuleId (RuleId), RulebookId (RulebookId))
import Effects.FileSystem (RelativePath (osPath), decodeOsPath)
import TypeScript.ModuleResolver (ModuleId (..))

newtype ProblemId = ProblemId
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (Hashable)

data Problem
    = LintProblem
        { lintRule :: LintRuleId
        , location :: Location
        , description :: Text
        , fix :: Text
        , autoFixable :: Bool
        }
    | RuleViolation
        { rulebook :: RulebookId
        , rule :: RuleId
        , badModule :: ModuleId
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

problemId :: Problem -> ProblemId
problemId
    LintProblem
        { lintRule = LintRuleId rId
        , location =
            Location
                { file = relPath
                }
        } = ProblemId $ rId <> "#" <> decodeOsPath relPath.osPath
problemId
    p@RuleViolation
        { rulebook = RulebookId rbId
        , rule = RuleId rId
        } =
        let
            mId = p.badModule.text
         in
            ProblemId $ rbId <> "#" <> rId <> "#" <> mId
