module Doubles.FileSystem (
    MockRoFileSystem (..),
    defaultMockRoFileSystem,
    runMockRoFileSystem,
) where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.FileSystem (
    AbsPath (..),
    RoFileSystem (..),
    absPathUnsafe,
    decodeOsPath,
    encodeOsPath,
 )
import System.OsPath (OsPath, (</>))

{- | A product type containing mock implementations for all RoFileSystem operations.
Parameterized over `es` so your mocks can utilize other effects in your test stack
(like Reader, State, or Writer) if needed.
-}
data MockRoFileSystem es = MockRoFileSystem
    { mockReadFile :: OsPath -> Eff es ByteString
    , mockReadAbsFile :: AbsPath -> Eff es ByteString
    , mockFileExists :: OsPath -> Eff es Bool
    , mockFileExistsAbs :: AbsPath -> Eff es Bool
    , mockDirectoryExists :: OsPath -> Eff es Bool
    , mockListDirectory :: OsPath -> Eff es [OsPath]
    , mockListAbsDirectory :: AbsPath -> Eff es [AbsPath]
    , mockIsDirectory :: OsPath -> Eff es Bool
    , mockIsAbsDirectory :: AbsPath -> Eff es Bool
    , mockGetHomeDirectory :: Eff es OsPath
    , mockMkAbsolute :: OsPath -> Eff es AbsPath
    }

-- | Sane and safe defaults for the mock file system.
defaultMockRoFileSystem :: MockRoFileSystem es
defaultMockRoFileSystem =
    MockRoFileSystem
        { mockReadFile = const $ pure mempty
        , mockReadAbsFile = const $ pure mempty
        , mockFileExists = const $ pure False
        , mockFileExistsAbs = const $ pure False
        , mockDirectoryExists = const $ pure False
        , mockListDirectory = const $ pure []
        , mockListAbsDirectory = const $ pure []
        , mockIsDirectory = const $ pure False
        , mockIsAbsDirectory = const $ pure False
        , mockGetHomeDirectory = pure $ encodeOsPath "~/"
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
    ReadAbsFile p -> mocks.mockReadAbsFile p
    FileExists p -> mocks.mockFileExists p
    FileExistsAbs p -> mocks.mockFileExistsAbs p
    DirectoryExists p -> mocks.mockDirectoryExists p
    ListDirectory p -> mocks.mockListDirectory p
    ListAbsDirectory p -> mocks.mockListAbsDirectory p
    IsDirectory p -> mocks.mockIsDirectory p
    IsAbsDirectory p -> mocks.mockIsAbsDirectory p
    GetHomeDirectory -> mocks.mockGetHomeDirectory
    MkAbsolute p -> mocks.mockMkAbsolute p
