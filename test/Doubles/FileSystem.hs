module Doubles.FileSystem (
    MockRoFileSystem (..),
    defaultMockRoFileSystem,
    runMockRoFileSystem,
    runReadOnly,
    runWritesInMemory,
    mockFiles,
    mockFileSystem,
    mockDirs,
    mockDirsWithSymlinks,
    mockFileAt,
    InMemoryFileSystem (..),
    newInMemoryFileSystem,
    runInMemoryFileSystem,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.FileSystem (RoFileSystem (..), WrFileSystem (..))
import FileSystem.Path (AbsPath (..), absPathUnsafe, decodeOsPath, encodeOsPath)
import System.OsPath (OsPath, splitDirectories, (</>))

{- | Every write lands in memory, keyed by the path it was written to, and
never reaches the disk: a run can read a real fixture while what it would have
written is asserted on instead. Writing a path again replaces what it held, as
on disk. Directories are implied by the files beneath them, so 'MkDirP'
records nothing.
-}
runWritesInMemory ::
    (IOE :> es) =>
    IORef (Map AbsPath ByteString) ->
    Eff (WrFileSystem : es) a ->
    Eff es a
runWritesInMemory ref = interpret $ \_ -> \case
    WriteFile path content -> liftIO . modifyIORef' ref $ Map.insert path content
    MkDirP _ -> pure ()

{- | A product type containing mock implementations for all RoFileSystem operations.
Parameterized over `es` so your mocks can utilize other effects in your test stack
(like Reader, State, or Writer) if needed.
-}
data MockRoFileSystem es = MockRoFileSystem
    { mockReadFile :: AbsPath -> Eff es ByteString
    , mockFileExists :: AbsPath -> Eff es Bool
    , mockDirectoryExists :: AbsPath -> Eff es Bool
    , mockIsSymlink :: AbsPath -> Eff es Bool
    , mockListDirectory :: AbsPath -> Eff es [AbsPath]
    , mockGetHomeDirectory :: Eff es AbsPath
    , mockMkAbsolute :: OsPath -> Eff es AbsPath
    }

-- | Sane and safe defaults for the mock file system.
defaultMockRoFileSystem :: MockRoFileSystem es
defaultMockRoFileSystem =
    MockRoFileSystem
        { mockReadFile = const $ pure mempty
        , mockFileExists = const $ pure False
        , mockDirectoryExists = const $ pure False
        , mockIsSymlink = const $ pure False
        , mockListDirectory = const $ pure []
        , mockGetHomeDirectory = pure . absPathUnsafe $ encodeOsPath "~/"
        , mockMkAbsolute = \p ->
            -- The (</>) operator safely ignores the left argument if `p` is already an absolute path
            let rawAbsPath = encodeOsPath "~/" </> p
             in pure . absPathUnsafe $ normalizeMockPath rawAbsPath
        }

-- | Purely mimics OS canonicalization for '.' and '..' paths.
normalizeMockPath :: OsPath -> OsPath
normalizeMockPath p =
    let txt = T.replace "\\" "/" . decodeOsPath $ p
        parts = T.splitOn "/" txt

        resolveDots acc "." = acc
        resolveDots [""] ".." = [""] -- Prevent backing out of root '/'
        resolveDots [] ".." = [".."] -- Preserve relative parent traversal
        resolveDots (_ : acc) ".." = acc
        resolveDots acc x = x : acc

        resolvedParts = reverse $ foldl' resolveDots [] parts
        -- Ensure root slash is preserved correctly if it was reduced to [""]
        finalParts = if resolvedParts == [""] then ["", ""] else resolvedParts
     in encodeOsPath $ T.intercalate "/" finalParts

-- | Runs a read-only computation over a mock, with nothing else in the stack.
runReadOnly :: MockRoFileSystem '[IOE] -> Eff '[RoFileSystem, IOE] a -> IO a
runReadOnly mocks = runEff . runMockRoFileSystem mocks

-- | Interprets the RoFileSystem effect using the provided mock configurations.
runMockRoFileSystem :: MockRoFileSystem es -> Eff (RoFileSystem : es) a -> Eff es a
runMockRoFileSystem mocks = interpret $ \_env -> \case
    ReadFile p -> mocks.mockReadFile p
    FileExists p -> mocks.mockFileExists p
    DirectoryExists p -> mocks.mockDirectoryExists p
    IsSymlink p -> mocks.mockIsSymlink p
    ListDirectory p -> mocks.mockListDirectory p
    GetHomeDirectory -> mocks.mockGetHomeDirectory
    MkAbsolute p -> mocks.mockMkAbsolute p

mockFiles :: [OsPath] -> MockRoFileSystem es
mockFiles existingFiles =
    defaultMockRoFileSystem
        { mockFileExists = \p -> pure $ p `elem` (absPathUnsafe <$> existingFiles)
        }

mockFileAt :: AbsPath -> ByteString -> MockRoFileSystem es
mockFileAt path content =
    defaultMockRoFileSystem
        { mockFileExists = \p -> pure (p == path)
        , mockReadFile = \_ -> pure content
        }

{- | An in-memory filesystem: exactly these paths exist, holding exactly these
bytes. A fake rather than a stub, so a component that reads several files - a
@tsconfig.json@ and everything it extends - runs its real codepath.
-}
mockFileSystem :: [(AbsPath, ByteString)] -> MockRoFileSystem es
mockFileSystem files =
    defaultMockRoFileSystem
        { mockFileExists = \p -> pure . any ((== p) . fst) $ files
        , mockReadFile = \p -> pure . maybe mempty snd . find ((== p) . fst) $ files
        }

-- | Build a mock that knows which paths are directories and what they contain.
mockDirs :: [(AbsPath, [AbsPath])] -> MockRoFileSystem es
mockDirs dirs =
    defaultMockRoFileSystem
        { mockDirectoryExists = \p -> pure . any ((== p) . fst) $ dirs
        , mockListDirectory = \p -> pure $ maybe [] snd (find ((== p) . fst) dirs)
        }

-- | As 'mockDirs', but with some of those directories reported as symlinks.
mockDirsWithSymlinks :: [(AbsPath, [AbsPath])] -> [AbsPath] -> MockRoFileSystem es
mockDirsWithSymlinks dirs symlinks =
    (mockDirs dirs) {mockIsSymlink = \p -> pure $ p `elem` symlinks}

{- | A working filesystem in memory: reads see what was written, and every write
is remembered, so a test can tell an untouched file from one rewritten with
the same bytes.

Directories are implied by the files beneath them.
-}
data InMemoryFileSystem = InMemoryFileSystem
    { files :: IORef (Map AbsPath ByteString)
    , written :: IORef [AbsPath]
    }

newInMemoryFileSystem :: (MonadIO m) => [(AbsPath, ByteString)] -> m InMemoryFileSystem
newInMemoryFileSystem initial =
    InMemoryFileSystem <$> newIORef (Map.fromList initial) <*> newIORef []

runInMemoryFileSystem ::
    (IOE :> es) =>
    InMemoryFileSystem ->
    Eff (WrFileSystem : RoFileSystem : es) a ->
    Eff es a
runInMemoryFileSystem fs = runMockRoFileSystem reading . writing
  where
    writing = interpret $ \_ -> \case
        WriteFile p content -> liftIO $ do
            modifyIORef' fs.files (Map.insert p content)
            modifyIORef' fs.written (<> [p])
        MkDirP _ -> pure ()

    reading =
        defaultMockRoFileSystem
            { mockReadFile = \p -> fromMaybe mempty . Map.lookup p <$> current
            , mockFileExists = \p -> Map.member p <$> current
            , mockDirectoryExists = \p -> any (isBeneath p) . Map.keys <$> current
            , mockListDirectory = \p -> ordNub . mapMaybe (childOf p) . Map.keys <$> current
            }

    current :: (IOE :> es') => Eff es' (Map AbsPath ByteString)
    current = readIORef fs.files

    segments = splitDirectories . (.osPath)
    isBeneath dir file = segments dir `isPrefixOf` segments file && segments dir /= segments file
    childOf dir file = case drop (length (segments dir)) (segments file) of
        child : _ | isBeneath dir file -> Just . absPathUnsafe $ dir.osPath </> child
        _ -> Nothing
