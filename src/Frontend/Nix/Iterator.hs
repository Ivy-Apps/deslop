{-# LANGUAGE QuasiQuotes #-}

module Frontend.Nix.Iterator (getNixFiles, getOtherFiles) where

import Effectful
import Effects.FileSystem (RoFileSystem)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (osPath), ProjectRoot (..))
import Git.Ignore (GitIgnore, isIgnored)
import System.OsPath (osp, takeExtension)

-- | Every @.nix@ file of the project that git would not ignore.
getNixFiles :: (RoFileSystem :> es) => GitIgnore -> ProjectRoot -> Eff es [AbsPath]
getNixFiles = filesWhere isNix

-- | Every other file of the project that git would not ignore.
getOtherFiles :: (RoFileSystem :> es) => GitIgnore -> ProjectRoot -> Eff es [AbsPath]
getOtherFiles = filesWhere (not . isNix)

filesWhere :: (RoFileSystem :> es) => (AbsPath -> Bool) -> GitIgnore -> ProjectRoot -> Eff es [AbsPath]
filesWhere wanted gitIgnore projectRoot = walkDir (isIgnored gitIgnore) file projectRoot.path
  where
    file entry = entry.path <$ guard (not entry.isDir && wanted entry.path)

isNix :: AbsPath -> Bool
isNix path = takeExtension path.osPath == [osp|.nix|]
