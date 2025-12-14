module Deslop (runDeslop) where

import Data.ByteString.Char8 qualified as C8
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, type (:>))
import Effects.FileSystem (FileSystem, readFileBS, runFileSystemIO)

program :: (FileSystem :> es, IOE :> es) => Eff es ()
program = do
  content <- readFileBS "deslop.cabal"
  liftIO $ putStrLn "File content:"
  liftIO $ C8.putStrLn content

runDeslop :: IO ()
runDeslop = do
  runEff $ do
    runFileSystemIO program