{-# LANGUAGE QuasiQuotes #-}

module Frontend.Haskell.Iterator (getHsFiles) where

import Effectful
import Effects.FileSystem (RoFileSystem, fsDirectoryExists)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (osPath))
import Frontend.Haskell.Cabal (Component (..), HsConfig (..))
import Git.Ignore (GitIgnore, isIgnored)
import System.OsPath (osp, takeExtension)

{- | Every @.hs@ file under any component's source directories that git would
not ignore, each once however many directories overlap. @.hs-boot@ and
literate @.lhs@ files are not read.
-}
getHsFiles :: (RoFileSystem :> es) => GitIgnore -> HsConfig -> Eff es [AbsPath]
getHsFiles gitIgnore cfg =
    fmap (sortNub . concat) . traverse walk . sortNub $ concatMap (toList . (.sourceDirs)) cfg.components
  where
    walk dir = fsDirectoryExists dir >>= bool (pure []) (walkDir (isIgnored gitIgnore) hsFile dir)
    hsFile entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.hs|])
