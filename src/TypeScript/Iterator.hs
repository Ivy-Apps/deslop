{-# LANGUAGE QuasiQuotes #-}

module TypeScript.Iterator (getTsFiles) where

import Effectful
import Effects.FileSystem (RoFileSystem, encodeOsPath, fsIsDirectory, fsListDirectory)
import System.OsPath (OsPath, osp, takeExtension, (</>))

getTsFiles :: (RoFileSystem :> es) => OsPath -> Eff es [OsPath]
getTsFiles dir = fsListDirectory dir >>= fmap concat . traverse (processEntry dir)
  where
    processEntry root entry
        | entry `elem` ignored = pure []
        | otherwise = resolve $ root </> entry

    resolve path = fsIsDirectory path >>= bool (tsOrEmpty path) (getTsFiles path)

    tsOrEmpty f = pure [f | takeExtension f `elem` [[osp|.ts|], [osp|.tsx|]]]
    ignored = map encodeOsPath ["node_modules", ".git", "dist", ".next"]
