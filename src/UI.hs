module UI (
    printErr,
    printSuccess,
    printDivider,
    printTitle,
    printTime,
    humanReadable,
    ProblemsLog (..),
    problemsLogText,
) where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effects.ReportProblem (Problem (..))
import Fmt
import System.Console.ANSI
import Types

newtype ProblemsLog = ProblemsLog [Problem]

instance Buildable ProblemsLog where
    build (ProblemsLog ps) =
        mconcat $
            intersperse "\n---------\n\n" (build <$> ps)

problemsLogText :: [Problem] -> Text
problemsLogText = T.pack . pretty . ProblemsLog

printErr :: Text -> IO ()
printErr err = do
    setSGR [SetColor Foreground Vivid Red]
    TIO.putStrLn $ "❌ Error: " <> err
    setSGR [Reset]

printSuccess :: Text -> IO ()
printSuccess msg = do
    setSGR [SetColor Foreground Vivid Green]
    TIO.putStrLn $ "✅ Success: " <> msg
    setSGR [Reset]

printDivider :: IO ()
printDivider = putStrLn "─────────────────────────────────────────"

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
    "tsconfig.json not found in '" <> T.pack path <> "'"
humanReadable (TsConfigParseError path) =
    "Could not parse TS config, check: '" <> T.pack path <> "'"
humanReadable CheckModeFoundProblems =
    "Problems found. Run without --check to fix."
