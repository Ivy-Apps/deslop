module UI (
    printErr,
    printDivider,
    printTitle,
    printTime,
    humanReadable,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fmt
import System.Console.ANSI
import Types

printErr :: Text -> IO ()
printErr err = do
    setSGR [SetColor Foreground Vivid Red]
    TIO.putStrLn $ "❌ Error: " <> err
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
