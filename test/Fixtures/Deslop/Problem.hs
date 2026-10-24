{- | Hand-built 'Problem's, for specs about what is done with one once found:
reporting, formatting, baselining, compacting.

Each builder fills in what no spec is about with a fixed placeholder, so a
spec states only what its assertion depends on. A placeholder that does matter
is replaced with one of the modifiers below: a record update would be partial,
since 'Problem' is a sum.
-}
module Fixtures.Deslop.Problem (
    -- * Lint Problems
    lintProblemAt,
    locatedAt,

    -- * Rule Violations
    BrokenRule (..),
    brokenRule,
    violationOf,
    importedBy,

    -- * Modifiers
    describedAs,
    withFix,
    autoFixable,
    reportedFrom,
) where

import Deslop.Module (EdgeKind (ImportEdge), Location (..), Module, moduleNameUnsafe)
import Deslop.Problem (LintRuleId (..), Problem (..), ViolationKind (..))
import Deslop.Rule.Book (RuleId (..), RulebookId (..))
import Fixtures.Deslop.Module (edgeTo)
import TestUtils (rp)

{- | A Lint Problem of the rule at the location, which @deslop fix@ cannot
repair.
-}
lintProblemAt :: Text -> Location -> Problem
lintProblemAt rule location =
    LintProblem
        { lintRule = LintRuleId rule
        , location
        , description = "Lint problem."
        , fix = "Fix it."
        , autoFixable = False
        }

-- | A statement in a file, at a line.
locatedAt :: Text -> Int -> Text -> Location
locatedAt file line code = Location {file = rp file, line, code}

{- | The Rule a violation is reported against, as its Rulebook wrote it. Its
fields are named apart from 'Problem''s, so that updating one of a 'Problem'
in a spec that imports both is never ambiguous.
-}
data BrokenRule = BrokenRule
    { rulebookId :: RulebookId
    , ruleId :: RuleId
    , ruleProse :: Text
    , ruleFix :: Text
    }

-- | A Rule of a Rulebook, with placeholder prose and fix.
brokenRule :: Text -> Text -> BrokenRule
brokenRule rulebook rule =
    BrokenRule
        { rulebookId = RulebookId rulebook
        , ruleId = RuleId rule
        , ruleProse = "Rule prose."
        , ruleFix = "Rule fix."
        }

{- | The module named @bad@ breaking the Rule. Its source sits at
@\<bad\>.ts@, as "Fixtures.Deslop.Module.mkModule" lays a module out.
-}
violationOf :: BrokenRule -> Text -> ViolationKind -> Problem
violationOf broken bad kind =
    RuleViolation
        { rulebook = broken.rulebookId
        , rule = broken.ruleId
        , badModule = moduleNameUnsafe bad
        , modulePath = rp (bad <> ".ts")
        , prose = broken.ruleProse
        , kind
        , fix = broken.ruleFix
        }

-- | The module importing @dep@ directly, at the line 'Fixtures.Deslop.Module.mkModule' gave it.
importedBy :: Module -> Text -> ViolationKind
importedBy m dep =
    DirectImport
        { imported = moduleNameUnsafe dep
        , edge = ImportEdge
        , location = edgeTo m dep
        }

-- | A Lint Problem described in other words. A Rule Violation's prose is its Rule's.
describedAs :: Text -> Problem -> Problem
describedAs description = \case
    p@LintProblem {} -> p {description}
    p@RuleViolation {} -> p

-- | The Problem with another fix, whichever kind it is.
withFix :: Text -> Problem -> Problem
withFix reworded = \case
    p@LintProblem {} -> p {fix = reworded}
    p@RuleViolation {} -> p {fix = reworded}

-- | A Lint Problem @deslop fix@ repairs. A Rule Violation never is one.
autoFixable :: Problem -> Problem
autoFixable = \case
    p@LintProblem {} -> p {autoFixable = True}
    p@RuleViolation {} -> p

-- | A Rule Violation by a module whose source sits elsewhere than 'violationOf' puts it.
reportedFrom :: Text -> Problem -> Problem
reportedFrom path = \case
    p@LintProblem {} -> p
    p@RuleViolation {} -> p {modulePath = rp path}
