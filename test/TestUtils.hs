module TestUtils (runFileSystemTest) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.FileSystem (FileSystem (FileExists, ReadFile, WriteFile))

runFileSystemTest ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (FileSystem : es) a ->
    Eff es a
runFileSystemTest ref = interpret $ \_ -> \case
    ReadFile path -> liftIO $ BS.readFile path
    WriteFile _path content -> liftIO $ writeIORef ref (Just content)
    FileExists _path -> pure True