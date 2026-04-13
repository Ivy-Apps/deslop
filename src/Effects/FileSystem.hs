module Effects.FileSystem (
    encodeOsPath,
    encodeOsPathString,
    decodeOsPath,
    absPathUnsafe,
    withAbsBaseUnsafe,
    withAbsBaseSafe,
    RoFileSystem (..),
    WrFileSystem (..),
    AbsPath (osPath),
    fsFileExists,
    fsReadFile,
    fsWriteFile,
    fsDirectoryExists,
    fsListDirectory,
    fsListAbsDirectory,
    fsIsDirectory,
    fsGetHomeDirectory,
    fsMkAbsolute,
    fsIsAbsDirectory,
    fsReadAbsFile,
    runFileSystemIO,
    runRoFileSystemIO,
) where

import Control.Monad.Catch.Pure (runCatch)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, decodeUtf, encodeUtf, (</>))

encodeOsPath :: Text -> OsPath
encodeOsPath = encodeOsPathString . T.unpack

encodeOsPathString :: FilePath -> OsPath
encodeOsPathString p =
    case runCatch (encodeUtf p) of
        Right path -> path
        Left err -> error $ "encodeOsPath failed: " <> show err

decodeOsPath :: OsPath -> Text
decodeOsPath = either handleErr T.pack . runCatch . decodeUtf
  where
    handleErr err = error $ "decodeOsPath failed: " <> show err

newtype AbsPath = AbsPath
    { osPath :: OsPath
    }
    deriving (Show, Eq)

absPathUnsafe :: OsPath -> AbsPath
absPathUnsafe = AbsPath

withAbsBaseUnsafe :: AbsPath -> OsPath -> AbsPath
withAbsBaseUnsafe (AbsPath b) p = AbsPath (b </> p)

withAbsBaseSafe :: AbsPath -> OsPath -> OsPath
withAbsBaseSafe (AbsPath b) p = b </> p

data RoFileSystem :: Effect where
    ReadFile :: OsPath -> RoFileSystem m ByteString
    ReadAbsFile :: AbsPath -> RoFileSystem m ByteString
    FileExists :: OsPath -> RoFileSystem m Bool
    DirectoryExists :: OsPath -> RoFileSystem m Bool
    ListDirectory :: OsPath -> RoFileSystem m [OsPath]
    ListAbsDirectory :: AbsPath -> RoFileSystem m [AbsPath]
    IsDirectory :: OsPath -> RoFileSystem m Bool
    IsAbsDirectory :: AbsPath -> RoFileSystem m Bool
    GetHomeDirectory :: RoFileSystem m OsPath
    MkAbsolute :: OsPath -> RoFileSystem m AbsPath

data WrFileSystem :: Effect where
    WriteFile :: OsPath -> ByteString -> WrFileSystem m ()

type instance DispatchOf RoFileSystem = Dynamic
type instance DispatchOf WrFileSystem = Dynamic

fsReadFile :: (RoFileSystem :> es) => OsPath -> Eff es ByteString
fsReadFile = send . ReadFile

fsReadAbsFile :: (RoFileSystem :> es) => AbsPath -> Eff es ByteString
fsReadAbsFile = send . ReadAbsFile

fsFileExists :: (RoFileSystem :> es) => OsPath -> Eff es Bool
fsFileExists = send . FileExists

fsDirectoryExists :: (RoFileSystem :> es) => OsPath -> Eff es Bool
fsDirectoryExists = send . DirectoryExists

fsListDirectory :: (RoFileSystem :> es) => OsPath -> Eff es [OsPath]
fsListDirectory = send . ListDirectory

fsListAbsDirectory :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
fsListAbsDirectory = send . ListAbsDirectory

fsIsDirectory :: (RoFileSystem :> es) => OsPath -> Eff es Bool
fsIsDirectory = send . IsDirectory

fsIsAbsDirectory :: (RoFileSystem :> es) => AbsPath -> Eff es Bool
fsIsAbsDirectory = send . IsAbsDirectory

fsGetHomeDirectory :: (RoFileSystem :> es) => Eff es OsPath
fsGetHomeDirectory = send GetHomeDirectory

fsWriteFile :: (WrFileSystem :> es) => OsPath -> ByteString -> Eff es ()
fsWriteFile path content = send $ WriteFile path content

fsMkAbsolute :: (RoFileSystem :> es) => OsPath -> Eff es AbsPath
fsMkAbsolute = send . MkAbsolute

runFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : RoFileSystem : es) a -> Eff es a
runFileSystemIO = runRoFileSystemIO . runWrFileSystemIO

runRoFileSystemIO :: (IOE :> es) => Eff (RoFileSystem : es) a -> Eff es a
runRoFileSystemIO = interpret $ \_env -> \case
    ReadFile path -> liftIO $ SFO.readFile' path
    ReadAbsFile (AbsPath path) -> liftIO $ SFO.readFile' path
    FileExists path -> liftIO $ SDO.doesFileExist path
    DirectoryExists path -> liftIO $ SDO.doesDirectoryExist path
    ListDirectory path -> liftIO $ SDO.listDirectory path
    ListAbsDirectory absP@(AbsPath p) ->
        liftIO
            . fmap (fmap (withAbsBaseUnsafe absP))
            . SDO.listDirectory
            $ p
    IsDirectory path -> liftIO $ SDO.doesDirectoryExist path
    IsAbsDirectory (AbsPath path) -> liftIO $ SDO.doesDirectoryExist path
    GetHomeDirectory -> liftIO SDO.getHomeDirectory
    MkAbsolute path -> liftIO . fmap AbsPath . SDO.canonicalizePath $ path

runWrFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : es) a -> Eff es a
runWrFileSystemIO = interpret $ \_env -> \case
    WriteFile path content -> liftIO $ SFO.writeFile' path content
