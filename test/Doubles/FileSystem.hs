module Doubles.FileSystem (
    MockRoFileSystem (..),
    defaultMockRoFileSystem,
    runMockRoFileSystem,
    runMockWrFileSystem,
    mockFiles,
    mockDirs,
) where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.FileSystem (
    AbsPath (..),
    RoFileSystem (..),
    WrFileSystem (..),
    absPathUnsafe,
    decodeOsPath,
    encodeOsPath,
 )
import System.OsPath (OsPath, (</>))

runMockWrFileSystem ::
    (IOE :> es) =>
    IORef (Maybe ByteString) ->
    Eff (WrFileSystem : es) a ->
    Eff es a
runMockWrFileSystem ref = interpret $ \_ -> \case
    WriteFile _path content -> liftIO $ writeIORef ref (Just content)

{- | A product type containing mock implementations for all RoFileSystem operations.
Parameterized over `es` so your mocks can utilize other effects in your test stack
(like Reader, State, or Writer) if needed.
-}
data MockRoFileSystem es = MockRoFileSystem
    { mockReadFile :: AbsPath -> Eff es ByteString
    , mockFileExists :: AbsPath -> Eff es Bool
    , mockDirectoryExists :: AbsPath -> Eff es Bool
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

-- | Interprets the RoFileSystem effect using the provided mock configurations.
runMockRoFileSystem :: MockRoFileSystem es -> Eff (RoFileSystem : es) a -> Eff es a
runMockRoFileSystem mocks = interpret $ \_env -> \case
    ReadFile p -> mocks.mockReadFile p
    FileExists p -> mocks.mockFileExists p
    DirectoryExists p -> mocks.mockDirectoryExists p
    ListDirectory p -> mocks.mockListDirectory p
    GetHomeDirectory -> mocks.mockGetHomeDirectory
    MkAbsolute p -> mocks.mockMkAbsolute p

mockFiles :: [OsPath] -> MockRoFileSystem es
mockFiles existingFiles =
    defaultMockRoFileSystem
        { mockFileExists = \p -> pure $ p `elem` (absPathUnsafe <$> existingFiles)
        }

-- | Build a mock that knows which paths are directories and what they contain.
mockDirs :: [(AbsPath, [AbsPath])] -> MockRoFileSystem es
mockDirs dirs =
    defaultMockRoFileSystem
        { mockDirectoryExists = \p -> pure . any ((== p) . fst) $ dirs
        , mockListDirectory = \p -> pure $ maybe [] snd (find ((== p) . fst) dirs)
        }
