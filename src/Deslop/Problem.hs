module Deslop.Problem (
    Problem (..),
    ViolationKind (..),
    ProblemId (..),
    problemId,
    isAutoFixable,
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
        , prose :: Text
        , kind :: ViolationKind
        , fix :: Text
        }
    deriving stock (Eq, Show, Ord)

{- | How a Rule was broken. The Rule's own prose says why the Rule exists; this
says what the module actually did, and carries the facts a report is written
from rather than the sentence itself - "Deslop.ProblemFormatter" owns that.
-}
data ViolationKind
    = -- | The module names the forbidden module in an import of its own.
      DirectImport
        { imported :: ModuleId
        , importStatement :: Text
        }
    | {- | The module arrives at a forbidden module by following imports.
      @chain@ runs from the module to what it must not reach, and @firstImport@
      is the import that opens it - absent when the chain has no first hop.
      -}
      TransitiveImport
        { chain :: NonEmpty ModuleId
        , firstImport :: Maybe Text
        , -- | The chains this violation stands in for, once duplicates have
          -- been compacted. Empty until "Deslop.ProblemShrinker" runs, and
          -- empty afterwards for a violation that had no duplicates.
          alsoReached :: [NonEmpty ModuleId]
        }
    | -- | The module does not import something the Rule requires it to.
      MissingUse
        { requiredImport :: Text
        , transitive :: Bool
        }
    | -- | A module the Rule requires to exist does not.
      MissingModule
        { requiredModule :: ModuleId
        }
    deriving stock (Eq, Show, Ord)

data Location = Location
    { file :: RelativePath
    , code :: Text
    }
    deriving stock (Eq, Show, Ord)

newtype LintRuleId = LintRuleId Text
    deriving stock (Eq, Show, Ord)

{- | Whether @deslop fix@ can resolve this Problem unattended.
A Rule Violation never is: rulebooks describe architecture, not rewrites.
-}
isAutoFixable :: Problem -> Bool
isAutoFixable LintProblem {autoFixable} = autoFixable
isAutoFixable RuleViolation {} = False

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
