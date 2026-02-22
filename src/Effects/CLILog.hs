module Effects.CLILog (
    CLILog (..),
    logModification,
    logSummary,
    logProblems,
    runCLILog,
) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.Function ((&))
import Effectful
import Effectful.Dispatch.Dynamic
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Effects.ReportProblem (Problem)

data CLILog :: Effect where
    LogModification :: FilePath -> CLILog m ()
    LogSummary :: CLILog m ()
    LogProblems :: [Problem] -> CLILog m ()

type instance DispatchOf CLILog = 'Dynamic

logModification :: (CLILog :> es) => FilePath -> Eff es ()
logModification = send . LogModification

logSummary :: (CLILog :> es) => Eff es ()
logSummary = send LogSummary

logProblems :: (CLILog :> es) => [Problem] -> Eff es ()
logProblems = send . LogProblems

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
                    putStrLn path
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
                LogProblems _ps -> liftIO $ do
                    putStrLn "TODO: Use the fmt Buildable instance to print ps"
            )

