module UI (
    putStderr,
    putStderrLn,
    printErr,
    printWarning,
    printSuccess,
    printDivider,
    printDividerStderr,
    printTitle,
    printTime,
    humanReadable,
    ProblemsLog (..),
    problemsLogText,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Deslop.Problem (Problem)
import Deslop.ProblemFormatter (formatProblem)
import Effects.FileSystem (decodeOsPath)
import Fmt
import System.Console.ANSI
import System.IO (hPutStr)
import Types

-- | ANSI SGR: red foreground
redCode :: String
redCode = "\x1b[31m"

-- | ANSI SGR: reset
resetCode :: String
resetCode = "\x1b[0m"

{- | Print a string to stderr in red. Single function for all stderr error output.
Include a trailing newline in the string if you want a line break.
-}
putStderr :: String -> IO ()
putStderr s = do
    hPutStr stderr redCode
    hPutStr stderr s
    hPutStr stderr resetCode

-- | Print a string to stderr in red, followed by a newline.
putStderrLn :: String -> IO ()
putStderrLn s = putStderr (s ++ "\n")

printWarning :: String -> IO ()
printWarning s = do
    setSGR [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
    putStrLn $ "WARNING: " <> s
    setSGR [Reset]

newtype ProblemsLog = ProblemsLog [Problem]

instance Buildable ProblemsLog where
    build (ProblemsLog ps) =
        mconcat $
            intersperse "\n---------\n\n" (build . formatProblem <$> ps)

problemsLogText :: [Problem] -> Text
problemsLogText = T.pack . pretty . ProblemsLog

printErr :: Text -> IO ()
printErr err = putStderrLn $ T.unpack ("❌ Error: " <> err)

printSuccess :: Text -> IO ()
printSuccess msg = do
    setSGR [SetColor Foreground Vivid Green]
    TIO.putStrLn $ "✅ Success: " <> msg
    setSGR [Reset]

printDivider :: IO ()
printDivider = putStrLn "─────────────────────────────────────────"

-- | Print divider to stderr in red (for problem/diagnostic output).
printDividerStderr :: IO ()
printDividerStderr = putStderrLn "─────────────────────────────────────────"

printTitle :: Text -> IO ()
printTitle t = do
    setSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
    TIO.putStrLn t
    setSGR [Reset]

printTime :: Double -> IO ()
printTime t = fmtLn $ "⏱  Finished in " +| formatDuration
  where
    formatDuration
        | t < 1 = fixedF 2 (t * 1000) |+ "ms"
        | otherwise = fixedF 2 t |+ "s"

humanReadable :: DeslopError -> Text
humanReadable (TsConfigNotFoundError path) =
    "tsconfig.json not found in '" <> decodeOsPath path <> "'"
humanReadable (TsConfigParseError path) =
    "Could not parse TS config, check: '" <> path <> "'"
humanReadable CheckModeFoundProblems =
    "Problems found. Run `deslop fix` to apply fixes."
humanReadable (RulebookErorr msg) =
    "Could not load Rulebook: " <> msg
humanReadable (InvalidRuleConfig msg) =
    "Invalid rule configuration: " <> msg
humanReadable CaptchaError =
    "Incorrect answer. Purchase a Deslop license at https://deslop.dev"
