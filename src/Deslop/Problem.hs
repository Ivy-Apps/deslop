module Deslop.Problem (
    Problem (..),
    ViolationKind (..),
    ProblemId (..),
    problemId,
    isAutoFixable,
    LintRuleId (..),
) where

import Deslop.Module (EdgeKind, Location (..), ModuleName (..))
import Deslop.Rule.Book (RuleId (RuleId), RulebookId (RulebookId))
import FileSystem.Path (ProjectRelativePath, portablePath)

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
        , badModule :: ModuleName
        , modulePath :: ProjectRelativePath
        -- ^ Where the module that broke the Rule lives. Every module has one,
        -- which is what lets the two 'ViolationKind's with no statement to
        -- point at still name a file.
        , prose :: Text
        , kind :: ViolationKind
        , fix :: Text
        }
    deriving stock (Eq, Show, Ord)

{- | How a Rule was broken. The Rule's own prose says why the Rule exists; this
says what the module actually did, and carries the facts a report is written
from rather than the sentence itself - "Deslop.Problem.Formatter" owns that.

Only the first two carry a 'Location', and that is the point of the sum: a
violation of /absence/ has nothing to quote, because the complaint is that
nobody wrote it. One optional location on the Problem would have to mean both
"this kind never has one" and "this one happens not to", which are different
facts and would render alike.
-}
data ViolationKind
    = {- | The module names the forbidden module in a dependency of its own.
      @edge@ says whether that dependency was an import or a re-export, so the
      sentence a report writes matches the statement quoted under it.
      -}
      DirectImport
        { imported :: ModuleName
        , edge :: EdgeKind
        , location :: Location
        }
    | {- | The module arrives at a forbidden module by following imports.
      @chain@ runs from the module to what it must not reach, and @firstImport@
      is the import that opens it - absent when the chain has no first hop.
      -}
      TransitiveImport
        { chain :: NonEmpty ModuleName
        , firstImport :: Maybe Location
        , -- | The chains this violation stands in for, once duplicates have
          -- been compacted. Empty until "Deslop.Problem.Shrinker" runs, and
          -- empty afterwards for a violation that had no duplicates.
          alsoReached :: [NonEmpty ModuleName]
        }
    | -- | The module does not import something the Rule requires it to.
      MissingUse
        { requiredImport :: Text
        , transitive :: Bool
        }
    | -- | A module the Rule requires to exist does not.
      MissingModule
        { requiredModule :: ModuleName
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

{- | What a Baseline remembers a Problem by.

Built from a file and a module name, and never from a line: an id that moved
when someone added a blank line above it would unsuppress every accepted
Problem below it on the next edit.
-}
problemId :: Problem -> ProblemId
problemId
    LintProblem
        { lintRule = LintRuleId rId
        , location =
            Location
                { file = relPath
                }
        } = ProblemId $ rId <> "#" <> portablePath relPath
problemId
    p@RuleViolation
        { rulebook = RulebookId rbId
        , rule = RuleId rId
        } =
        let
            mId = p.badModule.text
         in
            ProblemId $ rbId <> "#" <> rId <> "#" <> mId
