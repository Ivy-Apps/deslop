module Effects.FileSystem (
    readFileBS,
    writeFileBS,
    fileExists,
    directoryExists,
    listDirectory,
    isDirectory,
    getHomeDirectory,
    RoFileSystem (..),
    WrFileSystem (..),
    runFileSystemIO,
    runRoFileSystemIO,
) where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath)

data RoFileSystem :: Effect where
    ReadFile :: OsPath -> RoFileSystem m ByteString
    FileExists :: OsPath -> RoFileSystem m Bool
    DirectoryExists :: OsPath -> RoFileSystem m Bool
    ListDirectory :: OsPath -> RoFileSystem m [OsPath]
    IsDirectory :: OsPath -> RoFileSystem m Bool
    GetHomeDirectory :: RoFileSystem m OsPath

data WrFileSystem :: Effect where
    WriteFile :: OsPath -> ByteString -> WrFileSystem m ()

type instance DispatchOf RoFileSystem = Dynamic
type instance DispatchOf WrFileSystem = Dynamic

readFileBS :: (RoFileSystem :> es) => OsPath -> Eff es ByteString
readFileBS = send . ReadFile

fileExists :: (RoFileSystem :> es) => OsPath -> Eff es Bool
fileExists = send . FileExists

directoryExists :: (RoFileSystem :> es) => OsPath -> Eff es Bool
directoryExists = send . DirectoryExists

listDirectory :: (RoFileSystem :> es) => OsPath -> Eff es [OsPath]
listDirectory = send . ListDirectory

isDirectory :: (RoFileSystem :> es) => OsPath -> Eff es Bool
isDirectory = send . IsDirectory

getHomeDirectory :: (RoFileSystem :> es) => Eff es OsPath
getHomeDirectory = send GetHomeDirectory

writeFileBS :: (WrFileSystem :> es) => OsPath -> ByteString -> Eff es ()
writeFileBS path content = send $ WriteFile path content

runFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : RoFileSystem : es) a -> Eff es a
runFileSystemIO = runRoFileSystemIO . runWrFileSystemIO

runRoFileSystemIO :: (IOE :> es) => Eff (RoFileSystem : es) a -> Eff es a
runRoFileSystemIO = interpret $ \_env -> \case
    ReadFile path -> liftIO $ SFO.readFile' path
    FileExists path -> liftIO $ SDO.doesFileExist path
    DirectoryExists path -> liftIO $ SDO.doesDirectoryExist path
    ListDirectory path -> liftIO $ SDO.listDirectory path
    IsDirectory path -> liftIO $ SDO.doesDirectoryExist path
    GetHomeDirectory -> liftIO SDO.getHomeDirectory

runWrFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : es) a -> Eff es a
runWrFileSystemIO = interpret $ \_env -> \case
    WriteFile path content -> liftIO $ SFO.writeFile' path content
