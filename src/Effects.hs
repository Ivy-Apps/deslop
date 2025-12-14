module Effects (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m ByteString
  WriteFile :: FilePath -> ByteString -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

readFileBS :: (FileSystem :> es) => FilePath -> Eff es ByteString
readFileBS = send . ReadFile

writeFileBS :: (FileSystem :> es) => FilePath -> ByteString -> Eff es ()
writeFileBS path content = send $ WriteFile path content

runFileSystemIO :: (IOE :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_env -> \case
  ReadFile path -> liftIO $ BS.readFile path
  WriteFile path content -> liftIO $ BS.writeFile path content

program :: (FileSystem :> es, IOE :> es) => Eff es ()
program = do
  content <- readFileBS "deslop.cabal"
  liftIO . putStrLn $ "File content:"
  liftIO . putStrLn . show $ content

main :: IO ()
main = do
  runEff $ do
    runFileSystemIO program