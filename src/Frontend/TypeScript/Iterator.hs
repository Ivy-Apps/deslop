{-# LANGUAGE QuasiQuotes #-}

module Frontend.TypeScript.Iterator (getTsFiles) where

import Effectful
import Effects.FileSystem (RoFileSystem)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (osPath))
import Git.Ignore (GitIgnore, isIgnored)
import System.OsPath (osp, takeExtension)

{- | Every @.ts@ and @.tsx@ file in the project that git would not ignore.

Whatever the 'GitIgnore' ignores - always-ignored directories, what the
JavaScript ecosystem generates, gitignored paths - is pruned rather than
descended into, so it neither contributes files nor costs a traversal.
-}
getTsFiles :: (RoFileSystem :> es) => GitIgnore -> AbsPath -> Eff es [AbsPath]
getTsFiles gitIgnore = walkDir (isIgnored gitIgnore) tsFile
  where
    tsFile entry = entry.path <$ guard (not entry.isDir && isTs entry)
    isTs entry = takeExtension entry.path.osPath `elem` tsExtensions
    tsExtensions = [[osp|.ts|], [osp|.tsx|]]
