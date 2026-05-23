module Effects.CLI (
    CLI (..),
    logTitle,
    logModification,
    logFixSummary,
    logProblems,
    logNoProblemsFound,
    logBaselineSaved,
    logError,
    logWarning,
    logText,
    cliReadLine,
    runCLI,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Text qualified as T
import Deslop.Problem (Problem)
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.FileSystem (AbsPath (..), decodeOsPath)
import Fmt (fmt, pretty, (+|), (|+))
import Params (Command (..), Params (..))
import System.Console.ANSI
import UI (ProblemsLog (..), printDivider, printDividerStderr, printErr, printSuccess, printTitle, printWarning, putStderrLn)

data CLI :: Effect where
    LogTitle :: Params -> CLI m ()
    LogModification :: AbsPath -> CLI m ()
    LogFixSummary :: CLI m ()
    LogProblems :: [Problem] -> CLI m ()
    LogNoProblemsFound :: CLI m ()
    LogBaselineSaved :: Int -> CLI m ()
    LogError :: String -> CLI m ()
    LogWarning :: Text -> CLI m ()
    LogText :: Text -> CLI m ()
    ReadLine :: CLI m Text

type instance DispatchOf CLI = 'Dynamic

logTitle :: (CLI :> es) => Params -> Eff es ()
logTitle = send . LogTitle

logModification :: (CLI :> es) => AbsPath -> Eff es ()
logModification = send . LogModification

logFixSummary :: (CLI :> es) => Eff es ()
logFixSummary = send LogFixSummary

logProblems :: (CLI :> es) => [Problem] -> Eff es ()
logProblems = send . LogProblems

logNoProblemsFound :: (CLI :> es) => Eff es ()
logNoProblemsFound = send LogNoProblemsFound

logBaselineSaved :: (CLI :> es) => Int -> Eff es ()
logBaselineSaved = send . LogBaselineSaved

logError :: (CLI :> es) => String -> Eff es ()
logError = send . LogError

logWarning :: (CLI :> es) => Text -> Eff es ()
logWarning = send . LogWarning

logText :: (CLI :> es) => Text -> Eff es ()
logText = send . LogText

cliReadLine :: (CLI :> es) => Eff es Text
cliReadLine = send ReadLine

runCLI :: (IOE :> es) => Eff (CLI : es) a -> Eff es a
runCLI action = do
    counterVar <- liftIO $ newTVarIO (0 :: Int)

    action
        & interpret
            ( \_ -> \case
                LogTitle params -> do
                    let projectPath = decodeOsPath params.projectPath.osPath
                    liftIO . printTitle $ "🚀 Deslopping project: " <> projectPath
                    when (params.command == FixC) (liftIO . putStrLn $ "Changelog:")
                LogModification path -> liftIO $ do
                    atomically $ modifyTVar' counterVar (+ 1)
                    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
                    putStr "  modified  "
                    setSGR [Reset]
                    putStrLn (T.unpack . decodeOsPath $ path.osPath)
                    hFlush stdout
                LogFixSummary -> liftIO $ do
                    printDivider
                    count <- readTVarIO counterVar
                    setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
                    if count > 0
                        then
                            putStrLn $ "✨ Cleaned " ++ show count ++ " files successfully!"
                        else
                            putStrLn "✨ The project is already clean!"
                    setSGR [Reset]
                    printDivider
                LogProblems ps -> liftIO $ do
                    putStderrLn (fmt $ "Found " +| length ps |+ " problems:")
                    printDividerStderr
                    putStderrLn . pretty . ProblemsLog $ ps
                    printDividerStderr
                LogNoProblemsFound -> liftIO $ printSuccess "No problems found."
                LogBaselineSaved n ->
                    liftIO . printSuccess $
                        "Baseline generated with " <> T.pack (show n) <> " problem(s)."
                LogError e -> liftIO . printErr . T.pack $ e
                LogWarning t -> liftIO . printWarning . T.unpack $ t
                LogText t -> liftIO . putStrLn . T.unpack $ t
                ReadLine -> liftIO getLine
            )
