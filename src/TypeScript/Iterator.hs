{-# LANGUAGE QuasiQuotes #-}

module TypeScript.Iterator (getTsFiles) where

import Effectful
import Effects.FileSystem (AbsPath (osPath), RoFileSystem, encodeOsPath, fsDirectoryExists, fsListDirectory)
import System.OsPath (osp, takeExtension, takeFileName)

getTsFiles :: (RoFileSystem :> es) => AbsPath -> Eff es [AbsPath]
getTsFiles dir = fsListDirectory dir >>= fmap concat . traverse processEntry
  where
    tsExtensions = [[osp|.ts|], [osp|.tsx|]]
    ignored =
        fmap
            encodeOsPath
            [ "node_modules"
            , ".git"
            , "dist"
            , ".next"
            , "next-env.d.ts"
            , ".next-env.d.ts"
            , "build"
            , "out"
            , ".output"
            , "storybook-static"
            , "coverage"
            , ".direnv"
            , ".devenv"
            , ".turbo"
            , ".cache"
            , ".parcel-cache"
            , ".yarn"
            , ".svelte-kit"
            , ".nuxt"
            , ".astro"
            , ".vercel"
            , ".wrangler"
            ]

    processEntry entry
        | takeFileName entry.osPath `elem` ignored = pure []
        | otherwise = resolve entry

    resolve path = fsDirectoryExists path >>= bool (tsOrEmpty path) (getTsFiles path)

    tsOrEmpty f = pure [f | takeExtension f.osPath `elem` tsExtensions]
