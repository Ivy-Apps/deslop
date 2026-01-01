module Effects.FileSystem (
    readFileBS,
    writeFileBS,
    fileExists,
    listDirectory,
    isDirectory,
    FileSystem (..),
    runFileSystemIO,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Directory qualified as SD

data FileSystem :: Effect where
    ReadFile :: FilePath -> FileSystem m ByteString
    WriteFile :: FilePath -> ByteString -> FileSystem m ()
    FileExists :: FilePath -> FileSystem m Bool
    ListDirectory :: FilePath -> FileSystem m [FilePath]
    IsDirectory :: FilePath -> FileSystem m Bool

type instance DispatchOf FileSystem = Dynamic

readFileBS :: (FileSystem :> es) => FilePath -> Eff es ByteString
readFileBS = send . ReadFile

writeFileBS :: (FileSystem :> es) => FilePath -> ByteString -> Eff es ()
writeFileBS path content = send $ WriteFile path content

fileExists :: (FileSystem :> es) => FilePath -> Eff es Bool
fileExists = send . FileExists

listDirectory :: (FileSystem :> es) => FilePath -> Eff es [FilePath]
listDirectory = send . ListDirectory

isDirectory :: (FileSystem :> es) => FilePath -> Eff es Bool
isDirectory = send . IsDirectory

runFileSystemIO :: (IOE :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_env -> \case
    ReadFile path -> liftIO $ BS.readFile path
    WriteFile path content -> liftIO $ BS.writeFile path content
    FileExists path -> liftIO $ SD.doesFileExist path
    ListDirectory path -> liftIO $ SD.listDirectory path
    IsDirectory path -> liftIO $ SD.doesDirectoryExist path
