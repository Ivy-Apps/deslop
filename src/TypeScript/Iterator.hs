{-# LANGUAGE QuasiQuotes #-}

module TypeScript.Iterator (getTsFiles) where

import Effectful
import Effects.FileSystem (AbsPath (osPath), RoFileSystem)
import FileSystem.Iterator (Entry (..), walkDir)
import Git.Ignore (GitIgnore, alwaysIgnored, isIgnored)
import System.OsPath (osp, takeExtension)

{- | Every @.ts@ and @.tsx@ file in the project that git would not ignore.

Always-ignored directories are pruned regardless of what any @.gitignore@ says,
and gitignored directories are pruned rather than descended into, so neither
contributes files nor costs a traversal.
-}
getTsFiles :: (RoFileSystem :> es) => GitIgnore -> AbsPath -> Eff es [AbsPath]
getTsFiles gitIgnore = walkDir skip tsFile
  where
    skip entry = alwaysIgnored entry || isIgnored gitIgnore entry

    tsFile entry = entry.path <$ guard (not entry.isDir && isTs entry)
    isTs entry = takeExtension entry.path.osPath `elem` tsExtensions
    tsExtensions = [[osp|.ts|], [osp|.tsx|]]
