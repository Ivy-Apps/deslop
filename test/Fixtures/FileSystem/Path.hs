{- | The project every in-memory spec runs against, at @\/project@, so that a
path in one spec's fake filesystem means the same as in the next one's.
-}
module Fixtures.FileSystem.Path (
    projectRoot,
    inProject,
    fromProject,
    fromProjectText,
    projectFileSystem,
) where

import Data.Text qualified as T
import Doubles.FileSystem (InMemoryFileSystem, newInMemoryFileSystem)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath)
import TestUtils (ap)

projectRoot :: ProjectRoot
projectRoot = ProjectRoot (ap "/project")

-- | A path in the project, e.g. @inProject "src/a.ts"@.
inProject :: Text -> AbsPath
inProject = ap . ("/project/" <>)

-- | The inverse of 'inProject': a path by its path from the root, when it is in the project.
fromProject :: AbsPath -> Text
fromProject = fromProjectText . decodeOsPath . (.osPath)

-- | 'fromProject' for a path already spelled as text, such as a 'Deslop.Module.ModuleId'.
fromProjectText :: Text -> Text
fromProjectText text = fromMaybe text (T.stripPrefix "/project/" text)

-- | A file system holding exactly these files, by their path from the project root.
projectFileSystem :: [(Text, ByteString)] -> IO InMemoryFileSystem
projectFileSystem files = newInMemoryFileSystem [(inProject path, bytes) | (path, bytes) <- files]
