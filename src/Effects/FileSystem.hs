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
    fsGetHomeDirectory,
    fsMkAbsolute,
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
    ReadFile :: AbsPath -> RoFileSystem m ByteString
    FileExists :: AbsPath -> RoFileSystem m Bool
    DirectoryExists :: AbsPath -> RoFileSystem m Bool
    ListDirectory :: AbsPath -> RoFileSystem m [AbsPath]
    GetHomeDirectory :: RoFileSystem m AbsPath
    MkAbsolute :: OsPath -> RoFileSystem m AbsPath

data WrFileSystem :: Effect where
    WriteFile :: AbsPath -> ByteString -> WrFileSystem m ()

type instance DispatchOf RoFileSystem = Dynamic
type instance DispatchOf WrFileSystem = Dynamic

fsReadFile :: (RoFileSystem :> es) => AbsPath -> Eff es ByteString
fsReadFile = send . ReadFile

fsFileExists :: (RoFileSystem :> es) => AbsPath -> Eff es Bool
fsFileExists = send . FileExists

fsDirectoryExists :: (RoFileSystem :> es) => AbsPath -> Eff es Bool
fsDirectoryExists = send . DirectoryExists

fsListDirectory :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
fsListDirectory = send . ListDirectory

fsGetHomeDirectory :: (RoFileSystem :> es) => Eff es AbsPath
fsGetHomeDirectory = send GetHomeDirectory

fsWriteFile :: (WrFileSystem :> es) => AbsPath -> ByteString -> Eff es ()
fsWriteFile path content = send $ WriteFile path content

fsMkAbsolute :: (RoFileSystem :> es) => OsPath -> Eff es AbsPath
fsMkAbsolute = send . MkAbsolute

runFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : RoFileSystem : es) a -> Eff es a
runFileSystemIO = runRoFileSystemIO . runWrFileSystemIO

runRoFileSystemIO :: (IOE :> es) => Eff (RoFileSystem : es) a -> Eff es a
runRoFileSystemIO = interpret $ \_env -> \case
    ReadFile (AbsPath path) -> liftIO $ SFO.readFile' path
    FileExists (AbsPath path) -> liftIO $ SDO.doesFileExist path
    DirectoryExists (AbsPath path) -> liftIO $ SDO.doesDirectoryExist path
    ListDirectory absP@(AbsPath p) ->
        liftIO
            . fmap (fmap (withAbsBaseUnsafe absP))
            . SDO.listDirectory
            $ p
    GetHomeDirectory -> liftIO $ absPathUnsafe <$> SDO.getHomeDirectory
    MkAbsolute path -> liftIO . fmap AbsPath . SDO.canonicalizePath $ path

runWrFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : es) a -> Eff es a
runWrFileSystemIO = interpret $ \_env -> \case
    WriteFile (AbsPath path) content -> liftIO $ SFO.writeFile' path content
