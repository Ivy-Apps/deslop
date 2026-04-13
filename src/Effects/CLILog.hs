module Effects.CLILog (
    CLILog (..),
    logModification,
    logSummary,
    logProblems,
    logError,
    runCLILog,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.FileSystem (decodeOsPath)
import Effects.ReportProblem (Problem)
import Fmt (pretty)
import System.Console.ANSI
import System.OsPath (OsPath)
import UI (ProblemsLog (..), printErr, putStderrLn)

data CLILog :: Effect where
    LogModification :: OsPath -> CLILog m ()
    LogSummary :: CLILog m ()
    LogProblems :: [Problem] -> CLILog m ()
    LogError :: String -> CLILog m ()

type instance DispatchOf CLILog = 'Dynamic

logModification :: (CLILog :> es) => OsPath -> Eff es ()
logModification = send . LogModification

logSummary :: (CLILog :> es) => Eff es ()
logSummary = send LogSummary

logProblems :: (CLILog :> es) => [Problem] -> Eff es ()
logProblems = send . LogProblems

logError :: (CLILog :> es) => String -> Eff es ()
logError = send . LogError

runCLILog :: (IOE :> es) => Eff (CLILog : es) a -> Eff es a
runCLILog action = do
    counterVar <- liftIO $ newTVarIO (0 :: Int)

    action
        & interpret
            ( \_ -> \case
                LogModification path -> liftIO $ do
                    atomically $ modifyTVar' counterVar (+ 1)
                    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
                    putStr "  modified  "
                    setSGR [Reset]
                    putStrLn (T.unpack . decodeOsPath $ path)
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
                LogProblems ps -> liftIO . putStderrLn . pretty . ProblemsLog $ ps
                LogError e -> liftIO . printErr . T.pack $ e
            )
