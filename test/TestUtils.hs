module TestUtils (runFileSystemTest) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.FileSystem (RoFileSystem (..), WrFileSystem (..))

runFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : RoFileSystem : es) a ->
    Eff es a
runFileSystemTest ref = runRoFileSystemTest . runWrFileSystemTest ref

runRoFileSystemTest ::
    (IOE :> es) =>
    Eff (RoFileSystem : es) a ->
    Eff es a
runRoFileSystemTest = interpret $ \_ -> \case
    ReadFile path -> liftIO $ BS.readFile path
    FileExists _path -> pure True
    ListDirectory _path -> pure []
    IsDirectory _path -> pure False

runWrFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : es) a ->
    Eff es a
runWrFileSystemTest ref = interpret $ \_ -> \case
    WriteFile _path content -> liftIO $ writeIORef ref (Just content)
