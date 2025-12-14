module Deslop (deslopFile, runDeslop) where

import Data.ByteString (ByteString)
import Effectful (Eff, MonadIO (liftIO), runEff, type (:>))
import Effects.FileSystem (FileSystem, readFileBS, runFileSystemIO, writeFileBS)

deslopFile :: (FileSystem :> es) => FilePath -> FilePath -> Eff es ()
deslopFile inputPath outputPath = do
  original <- readFileBS inputPath
  let clean = removeSlop original
  writeFileBS outputPath clean

removeSlop :: ByteString -> ByteString
removeSlop = id

runDeslop :: IO ()
runDeslop = runEff $ runFileSystemIO $ do
  deslopFile "test/fixtures/typescript/input/01-comments.ts" "demo.ts"
  liftIO $ putStrLn "Deslop complete ✅"