module Effects.FileSystem (
    readFileBS,
    writeFileBS,
    fileExists,
    fullPath,
    FileSystem (ReadFile, WriteFile, FileExists, FullPath),
    runFileSystemIO,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Directory (canonicalizePath, doesFileExist)

data FileSystem :: Effect where
    ReadFile :: FilePath -> FileSystem m ByteString
    WriteFile :: FilePath -> ByteString -> FileSystem m ()
    FileExists :: FilePath -> FileSystem m Bool
    FullPath :: FilePath -> FileSystem m FilePath

type instance DispatchOf FileSystem = Dynamic

readFileBS :: (FileSystem :> es) => FilePath -> Eff es ByteString
readFileBS = send . ReadFile

writeFileBS :: (FileSystem :> es) => FilePath -> ByteString -> Eff es ()
writeFileBS path content = send $ WriteFile path content

fileExists :: (FileSystem :> es) => FilePath -> Eff es Bool
fileExists = send . FileExists

fullPath :: (FileSystem :> es) => FilePath -> Eff es FilePath
fullPath = send . FullPath

runFileSystemIO :: (IOE :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_env -> \case
    ReadFile path -> liftIO $ BS.readFile path
    WriteFile path content -> liftIO $ BS.writeFile path content
    FileExists path -> liftIO $ doesFileExist path
    FullPath path -> liftIO $ canonicalizePath path