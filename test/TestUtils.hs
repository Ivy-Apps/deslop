module TestUtils (
    runFileSystemTest,
    runCLILogTest,
    runGitTest,
    defaultParams,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Deslop (Params (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effects.CLILog
import Effects.FileSystem (RoFileSystem (..), WrFileSystem (..))
import Effects.Git

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

runCLILogTest :: Eff (CLILog : es) a -> Eff es a
runCLILogTest = interpret $ \_ -> \case
    LogModification _ -> pure ()
    LogSummary -> pure ()

defaultParams :: FilePath -> Params
defaultParams projPath =
    Params
        { projectPath = projPath
        , imports = True
        , comments = True
        , modified = False
        }

runGitTest :: Eff (Git : es) a -> Eff es a
runGitTest = interpret $ \_ -> \case
    ModifiedFiles -> pure []