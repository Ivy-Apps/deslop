{- | Everything Deslop says to the user, as 'Text'.

Pure by design: this module composes sentences and nothing else. Colouring one
and putting it on a stream is "Effects.CLI", which is the only place in the
codebase that writes to a terminal.
-}
module UI (
    divider,
    summaryLine,
    coverage,
    humanReadable,
    problemsFoundText,
    ProblemsLog (..),
    problemsLogText,
) where

import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime)
import Deslop.Error (DeslopError (..))
import Deslop.Problem (Problem)
import Deslop.Problem.Formatter (formatProblem)
import Deslop.RunReport (ModuleCount (..), ProblemCounts (..), RuleCount (..), RunSummary (..))
import FileSystem.Path (decodeOsPath)
import Fmt
import Utils (pluralise)

divider :: Text
divider = "─────────────────────────────────────────"

{- | The closing line of a run:
@"⏱  Checked 412 modules enforcing 38 rules in 870ms"@.
-}
summaryLine :: RunSummary -> NominalDiffTime -> Text
summaryLine summary d = "⏱  " <> coverage summary <> " in " <> duration d

-- | What the run went through: @"Checked 412 modules enforcing 38 rules"@.
coverage :: RunSummary -> Text
coverage (Checked ms rs) = "Checked " <> modules ms <> " enforcing " <> rules rs
coverage (Baselined ms rs) = "Baselined " <> modules ms <> " enforcing " <> rules rs
coverage (Scanned ms) = "Scanned " <> modules ms

modules :: ModuleCount -> Text
modules (ModuleCount n) = pluralise n "module"

rules :: RuleCount -> Text
rules (RuleCount n) = pluralise n "rule"

-- | Whole milliseconds below a second, seconds above it.
duration :: NominalDiffTime -> Text
duration d
    | t < 1 = show (round (t * 1000) :: Int) <> "ms"
    | otherwise = fmt $ fixedF 2 t |+ "s"
  where
    t = realToFrac d :: Double

newtype ProblemsLog = ProblemsLog [Problem]

instance Buildable ProblemsLog where
    build (ProblemsLog ps) =
        mconcat $
            intersperse "\n---------\n\n" (build . formatProblem <$> ps)

problemsLogText :: [Problem] -> Text
problemsLogText = T.pack . pretty . ProblemsLog

humanReadable :: DeslopError -> Text
humanReadable (TsConfigNotFoundError path) =
    "tsconfig.json not found in '" <> decodeOsPath path <> "'"
humanReadable (TsConfigParseError path) =
    "Could not parse TS config, check: '" <> path <> "'"
-- The loader's report already names every file, rule and field, and counts
-- them, so a prefix here would only say it twice.
humanReadable (RulebookError msg) = msg
humanReadable (InvalidRuleConfig msg) =
    "Invalid rule configuration: " <> msg

-- | What the user can do about the Problems a check found.
problemsFoundText :: ProblemCounts -> Text
problemsFoundText counts =
    T.intercalate "\n" $ headline : fixLine <> [baselineLine]
  where
    headline =
        "Found " <> pluralise counts.total "problem" <> ", " <> fixableCount <> "."
    fixableCount = case counts.autoFixable of
        0 -> "none auto-fixable"
        n -> show n <> " of them auto-fixable"
    fixLine = case counts.autoFixable of
        0 -> []
        n ->
            [ "   Run `deslop fix` to fix the "
                <> pluralise n "auto-fixable problem"
                <> "."
            ]
    baselineLine =
        "   Run `deslop baseline` to silence all "
            <> pluralise counts.total "problem"
            <> "."
