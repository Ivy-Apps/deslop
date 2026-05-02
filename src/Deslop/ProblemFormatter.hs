module Deslop.ProblemFormatter (formatProblem) where

import Data.Text qualified as T
import Deslop.Problem (Location (..), Problem (..), ProblemId (..), problemId)

formatProblem :: Problem -> Text
formatProblem p@LintProblem {} =
    autoFix <> problemHeader <> description <> code <> fixText
  where
    autoFix = bool "" "[AUTO-FIXABLE] " p.autoFixable
    problemHeader = "# " <> (problemId p).text <> "\n"
    description = p.description <> "\n"
    code = "```ts\n" <> T.strip p.location.code <> "\n```\n"
    fixText = "FIX: " <> T.strip p.fix
formatProblem p@RuleViolation {} =
    problemHeader <> description <> fixText
  where
    problemHeader = "# " <> (problemId p).text <> "\n"
    description = p.description <> "\n"
    fixText = "FIX: " <> T.strip p.fix
