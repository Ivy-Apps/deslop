module UI (
    blueBold,
    green,
    yellowBold,
    cyanBold,
    redStderr,
    plainOut,
    divider,
    summaryLine,
    coverage,
    pluralise,
    humanReadable,
    problemsFoundText,
    ProblemsLog (..),
    problemsLogText,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime)
import Deslop.Problem (Problem)
import Deslop.ProblemFormatter (formatProblem)
import Effects.FileSystem (decodeOsPath)
import Fmt
import System.Console.ANSI
import System.IO (hPutStr)
import Types (DeslopError (..), ModuleCount (..), ProblemCounts (..), RuleCount (..), RunSummary (..))

--------------------------------------------------------------------------------
-- Colour primitives
--------------------------------------------------------------------------------

blueBold :: Text -> IO ()
blueBold = withSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]

green :: Text -> IO ()
green = withSGR [SetColor Foreground Vivid Green]

yellowBold :: Text -> IO ()
yellowBold = withSGR [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]

cyanBold :: Text -> IO ()
cyanBold = withSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]

plainOut :: Text -> IO ()
plainOut t = TIO.putStrLn t >> hFlush stdout

withSGR :: [SGR] -> Text -> IO ()
withSGR sgr t = do
    setSGR sgr
    TIO.putStrLn t
    setSGR [Reset]
    hFlush stdout

{- | Print to stderr in red. ANSI codes are written raw rather than via 'setSGR',
which only ever targets stdout.
-}
redStderr :: Text -> IO ()
redStderr t = do
    hPutStr stderr redCode
    hPutStr stderr (T.unpack t)
    hPutStr stderr resetCode
    hPutStr stderr "\n"
  where
    redCode = "\x1b[31m"
    resetCode = "\x1b[0m"

--------------------------------------------------------------------------------
-- Pure text helpers
--------------------------------------------------------------------------------

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

-- | @pluralise 1 "rule" == "1 rule"@, @pluralise 2 "rule" == "2 rules"@.
pluralise :: Int -> Text -> Text
pluralise 1 word = "1 " <> word
pluralise n word = show n <> " " <> word <> "s"

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
humanReadable (RulebookError msg) =
    "Could not load Rulebook: " <> msg
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
