module Effects.CLILog (
    CLILog (..),
    logTitle,
    logModification,
    logSummary,
    logProblems,
    logNoProblemsFound,
    logError,
    runCLILog,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Text qualified as T
import Deslop.Problem (Problem)
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.FileSystem (AbsPath (..), decodeOsPath)
import Fmt (fmt, pretty, (+|), (|+))
import Params (Params (..))
import System.Console.ANSI
import UI (ProblemsLog (..), printDividerStderr, printErr, printSuccess, printTitle, putStderrLn)

data CLILog :: Effect where
    LogTitle :: Params -> CLILog m ()
    LogModification :: AbsPath -> CLILog m ()
    LogSummary :: CLILog m ()
    LogProblems :: [Problem] -> CLILog m ()
    LogNoProblemsFound :: CLILog m ()
    LogError :: String -> CLILog m ()

type instance DispatchOf CLILog = 'Dynamic

logTitle :: (CLILog :> es) => Params -> Eff es ()
logTitle = send . LogTitle

logModification :: (CLILog :> es) => AbsPath -> Eff es ()
logModification = send . LogModification

logSummary :: (CLILog :> es) => Eff es ()
logSummary = send LogSummary

logProblems :: (CLILog :> es) => [Problem] -> Eff es ()
logProblems = send . LogProblems

logNoProblemsFound :: (CLILog :> es) => Eff es ()
logNoProblemsFound = send LogNoProblemsFound

logError :: (CLILog :> es) => String -> Eff es ()
logError = send . LogError

runCLILog :: (IOE :> es) => Eff (CLILog : es) a -> Eff es a
runCLILog action = do
    counterVar <- liftIO $ newTVarIO (0 :: Int)

    action
        & interpret
            ( \_ -> \case
                LogTitle params -> do
                    let projectPath = decodeOsPath params.projectPath.osPath
                    liftIO . printTitle $ "🚀 Deslopping project: " <> projectPath
                LogModification path -> liftIO $ do
                    atomically $ modifyTVar' counterVar (+ 1)
                    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
                    putStr "  modified  "
                    setSGR [Reset]
                    putStrLn (T.unpack . decodeOsPath $ path.osPath)
                    hFlush stdout
                LogSummary -> liftIO $ do
                    count <- readTVarIO counterVar
                    setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
                    if count > 0
                        then
                            putStrLn $ "✨ Cleaned " ++ show count ++ " files successfully!"
                        else
                            putStrLn "✨ The project is already clean!"
                    setSGR [Reset]
                LogProblems ps -> liftIO $ do
                    putStderrLn (fmt $ "Found " +| length ps |+ " problems:")
                    printDividerStderr
                    putStderrLn . pretty . ProblemsLog $ ps
                    printDividerStderr
                LogNoProblemsFound -> liftIO $ printSuccess "No problems found."
                LogError e -> liftIO . printErr . T.pack $ e
            )
