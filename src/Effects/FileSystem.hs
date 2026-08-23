{- | Reaching the filesystem, split read from write so that a component which
only inspects a project cannot rewrite it.

The path types themselves are "FileSystem.Path", which is pure: a module that
merely names a path should not have to take on an effect to do it.
-}
module Effects.FileSystem (
    RoFileSystem (..),
    WrFileSystem (..),
    fsFileExists,
    fsReadFile,
    fsWriteFile,
    fsMkDirP,
    fsDirectoryExists,
    fsIsSymlink,
    fsListDirectory,
    fsGetHomeDirectory,
    fsMkAbsolute,
    runFileSystemIO,
    runRoFileSystemIO,
) where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import FileSystem.Path (AbsPath (osPath), absPathUnsafe, withAbsBaseUnsafe)
import System.Directory.OsPath qualified as SDO
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath)

data RoFileSystem :: Effect where
    ReadFile :: AbsPath -> RoFileSystem m ByteString
    FileExists :: AbsPath -> RoFileSystem m Bool
    DirectoryExists :: AbsPath -> RoFileSystem m Bool
    IsSymlink :: AbsPath -> RoFileSystem m Bool
    ListDirectory :: AbsPath -> RoFileSystem m [AbsPath]
    GetHomeDirectory :: RoFileSystem m AbsPath
    MkAbsolute :: OsPath -> RoFileSystem m AbsPath

data WrFileSystem :: Effect where
    WriteFile :: AbsPath -> ByteString -> WrFileSystem m ()
    MkDirP :: AbsPath -> WrFileSystem m ()

type instance DispatchOf RoFileSystem = Dynamic
type instance DispatchOf WrFileSystem = Dynamic

fsReadFile :: (RoFileSystem :> es) => AbsPath -> Eff es ByteString
fsReadFile = send . ReadFile

fsFileExists :: (RoFileSystem :> es) => AbsPath -> Eff es Bool
fsFileExists = send . FileExists

fsDirectoryExists :: (RoFileSystem :> es) => AbsPath -> Eff es Bool
fsDirectoryExists = send . DirectoryExists

{- | Whether the path is a symbolic link, without following it.

Directory walks use this to refuse to descend into symlinked directories,
which is what git itself does and what makes those walks structurally
terminating: a symlink loop yields ever-longer distinct paths, so a
visited-set over raw paths would never detect it.
-}
fsIsSymlink :: (RoFileSystem :> es) => AbsPath -> Eff es Bool
fsIsSymlink = send . IsSymlink

fsListDirectory :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
fsListDirectory = send . ListDirectory

fsGetHomeDirectory :: (RoFileSystem :> es) => Eff es AbsPath
fsGetHomeDirectory = send GetHomeDirectory

fsWriteFile :: (WrFileSystem :> es) => AbsPath -> ByteString -> Eff es ()
fsWriteFile path content = send $ WriteFile path content

fsMkDirP :: (WrFileSystem :> es) => AbsPath -> Eff es ()
fsMkDirP = send . MkDirP

fsMkAbsolute :: (RoFileSystem :> es) => OsPath -> Eff es AbsPath
fsMkAbsolute = send . MkAbsolute

runFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : RoFileSystem : es) a -> Eff es a
runFileSystemIO = runRoFileSystemIO . runWrFileSystemIO

runRoFileSystemIO :: (IOE :> es) => Eff (RoFileSystem : es) a -> Eff es a
runRoFileSystemIO = interpret $ \_env -> \case
    ReadFile p -> liftIO $ SFO.readFile' p.osPath
    FileExists p -> liftIO $ SDO.doesFileExist p.osPath
    DirectoryExists p -> liftIO $ SDO.doesDirectoryExist p.osPath
    IsSymlink p -> liftIO $ SDO.pathIsSymbolicLink p.osPath
    -- Sorted, because listDirectory's order is unspecified and every caller
    -- turns it into output a user reads: which rulebooks load first, which
    -- modules are walked first. Left to the filesystem, that order differs
    -- between machines and the same run gives two different answers.
    ListDirectory p ->
        liftIO
            . fmap (sort . fmap (withAbsBaseUnsafe p))
            . SDO.listDirectory
            $ p.osPath
    GetHomeDirectory -> liftIO $ absPathUnsafe <$> SDO.getHomeDirectory
    MkAbsolute path -> liftIO . fmap absPathUnsafe . SDO.canonicalizePath $ path

runWrFileSystemIO :: (IOE :> es) => Eff (WrFileSystem : es) a -> Eff es a
runWrFileSystemIO = interpret $ \_env -> \case
    WriteFile p content -> liftIO $ SFO.writeFile' p.osPath content
    MkDirP p -> liftIO $ SDO.createDirectoryIfMissing True p.osPath
