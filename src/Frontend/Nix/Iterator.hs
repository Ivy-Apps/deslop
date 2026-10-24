{-# LANGUAGE QuasiQuotes #-}

module Frontend.Nix.Iterator (getNixFiles) where

import Effectful
import Effects.FileSystem (RoFileSystem)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (osPath), ProjectRoot (..))
import Git.Ignore (GitIgnore, isIgnored)
import System.OsPath (osp, takeExtension)

-- | Every @.nix@ file of the project that git would not ignore.
getNixFiles :: (RoFileSystem :> es) => GitIgnore -> ProjectRoot -> Eff es [AbsPath]
getNixFiles gitIgnore projectRoot = walkDir (isIgnored gitIgnore) nixFile projectRoot.path
  where
    nixFile entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.nix|])
