module Effects.FileSystem (
    readFileBS,
    writeFileBS,
    fileExists,
    listDirectory,
    isDirectory,
    RoFileSystem (..),
    WrFileSystem (..),
    runFileSystemIO,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Directory qualified as SD

data RoFileSystem :: Effect where
    ReadFile :: FilePath -> RoFileSystem m ByteString
    FileExists :: FilePath -> RoFileSystem m Bool
    ListDirectory :: FilePath -> RoFileSystem m [FilePath]
    IsDirectory :: FilePath -> RoFileSystem m Bool

data WrFileSystem :: Effect where
    WriteFile :: FilePath -> ByteString -> WrFileSystem m ()

type instance DispatchOf RoFileSystem = Dynamic
type instance DispatchOf WrFileSystem = Dynamic

readFileBS :: (RoFileSystem :> es) => FilePath -> Eff es ByteString
readFileBS = send . ReadFile

fileExists :: (RoFileSystem :> es) => FilePath -> Eff es Bool
fileExists = send . FileExists

listDirectory :: (RoFileSystem :> es) => FilePath -> Eff es [FilePath]
listDirectory = send . ListDirectory

isDirectory :: (RoFileSystem :> es) => FilePath -> Eff es Bool
isDirectory = send . IsDirectory

writeFileBS :: (WrFileSystem :> es) => FilePath -> ByteString -> Eff es ()
writeFileBS path content = send $ WriteFile path content

runFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : RoFileSystem : es) a -> Eff es a
runFileSystemIO = runRoFileSystemIO . runWrFileSystemIO

runRoFileSystemIO :: (IOE :> es) => Eff (RoFileSystem : es) a -> Eff es a
runRoFileSystemIO = interpret $ \_env -> \case
    ReadFile path -> liftIO $ BS.readFile path
    FileExists path -> liftIO $ SD.doesFileExist path
    ListDirectory path -> liftIO $ SD.listDirectory path
    IsDirectory path -> liftIO $ SD.doesDirectoryExist path

runWrFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : es) a -> Eff es a
runWrFileSystemIO = interpret $ \_env -> \case
    WriteFile path content -> liftIO $ BS.writeFile path content
