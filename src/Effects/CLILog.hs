module Effects.CLILog where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Data.Function ((&))
import Effectful
import Effectful.Dispatch.Dynamic
import System.Console.ANSI
import System.IO (hFlush, stdout)

data CLILog :: Effect where
    LogModification :: FilePath -> CLILog m ()
    LogSummary :: CLILog m ()

type instance DispatchOf CLILog = 'Dynamic

logModification :: (CLILog :> es) => FilePath -> Eff es ()
logModification = send . LogModification

logSummary :: (CLILog :> es) => Eff es ()
logSummary = send LogSummary

runCLILog :: (IOE :> es) => Eff (CLILog : es) a -> Eff es a
runCLILog action = do
    counterVar <- liftIO $ newTVarIO (0 :: Int)

    action
        & ( interpret $ \_ -> \case
                LogModification path -> liftIO $ do
                    atomically $ modifyTVar' counterVar (+ 1)
                    setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
                    putStr "  modified  "
                    setSGR [Reset]
                    putStrLn path
                    hFlush stdout
                LogSummary -> liftIO $ do
                    count <- atomically $ readTVar counterVar
                    setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
                    if count > 0
                        then
                            putStrLn $ "✨ Cleaned " ++ show count ++ " files successfully!"
                        else
                            putStrLn $ "✨ The project is already clean!"
                    setSGR [Reset]
          )