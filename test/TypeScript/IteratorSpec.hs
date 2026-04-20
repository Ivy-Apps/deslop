{-# LANGUAGE QuasiQuotes #-}

module TypeScript.IteratorSpec (spec) where

import Doubles.FileSystem (mockDirs, runMockRoFileSystem)
import Effectful (runEff)
import Effects.FileSystem (AbsPath, absPathUnsafe, encodeOsPath)
import Test.Hspec
import TypeScript.Iterator (getTsFiles)

ap :: Text -> AbsPath
ap = absPathUnsafe . encodeOsPath

spec :: Spec
spec = describe "TypeScript.Iterator" $ do
    let root = ap "/project"

    it "returns empty list for empty directory" $ do
        result <- runEff . runMockRoFileSystem (mockDirs [(root, [])]) $ getTsFiles root
        result `shouldBe` []

    it "collects multiple .ts and .tsx files from a flat directory" $ do
        let files =
                [ ap "/project/index.ts"
                , ap "/project/App.tsx"
                , ap "/project/utils.ts"
                , ap "/project/Button.tsx"
                ]
        result <- runEff . runMockRoFileSystem (mockDirs [(root, files)]) $ getTsFiles root
        result `shouldBe` files

    it "excludes non-TypeScript files alongside .ts/.tsx files" $ do
        let tsFile = ap "/project/index.ts"
        let tsxFile = ap "/project/App.tsx"
        let allEntries =
                [ tsFile
                , ap "/project/styles.css"
                , tsxFile
                , ap "/project/README.md"
                , ap "/project/build.js"
                ]
        result <- runEff . runMockRoFileSystem (mockDirs [(root, allEntries)]) $ getTsFiles root
        result `shouldBe` [tsFile, tsxFile]

    it "recurses into a subdirectory and collects .ts and .tsx files" $ do
        let srcDir = ap "/project/src"
        let files =
                [ ap "/project/src/main.ts"
                , ap "/project/src/App.tsx"
                , ap "/project/src/index.ts"
                ]
        result <-
            runEff . runMockRoFileSystem (mockDirs [(root, [srcDir]), (srcDir, files)]) $
                getTsFiles root
        result `shouldBe` files

    it "recurses deeply (3 levels) and collects all .ts/.tsx files" $ do
        let componentsDir = ap "/project/src/components"
        let srcDir = ap "/project/src"
        let rootTs = ap "/project/root.ts"
        let srcTs = ap "/project/src/service.ts"
        let compTs = ap "/project/src/components/Button.tsx"
        let compTs2 = ap "/project/src/components/Modal.ts"
        result <-
            runEff
                . runMockRoFileSystem
                    ( mockDirs
                        [ (root, [rootTs, srcDir])
                        , (srcDir, [srcTs, componentsDir])
                        , (componentsDir, [compTs, compTs2])
                        ]
                    )
                $ getTsFiles root
        result `shouldBe` [rootTs, srcTs, compTs, compTs2]

    it "skips node_modules" $ do
        let nodeModules = ap "/project/node_modules"
        let srcFile = ap "/project/index.ts"
        result <-
            runEff
                . runMockRoFileSystem
                    ( mockDirs
                        [ (root, [nodeModules, srcFile])
                        , (nodeModules, [ap "/project/node_modules/dep.ts"])
                        ]
                    )
                $ getTsFiles root
        result `shouldBe` [srcFile]

    it "skips .git" $ do
        let gitDir = ap "/project/.git"
        result <-
            runEff
                . runMockRoFileSystem
                    ( mockDirs
                        [ (root, [gitDir])
                        , (gitDir, [ap "/project/.git/hook.ts"])
                        ]
                    )
                $ getTsFiles root
        result `shouldBe` []

    it "skips dist" $ do
        let distDir = ap "/project/dist"
        result <-
            runEff
                . runMockRoFileSystem
                    ( mockDirs
                        [ (root, [distDir])
                        , (distDir, [ap "/project/dist/bundle.ts", ap "/project/dist/chunk.tsx"])
                        ]
                    )
                $ getTsFiles root
        result `shouldBe` []

    it "skips .next" $ do
        let nextDir = ap "/project/.next"
        result <-
            runEff
                . runMockRoFileSystem
                    ( mockDirs
                        [ (root, [nextDir])
                        , (nextDir, [ap "/project/.next/server.ts"])
                        ]
                    )
                $ getTsFiles root
        result `shouldBe` []

    it "collects files from sibling directories while skipping all ignored ones" $ do
        let nodeModules = ap "/project/node_modules"
        let distDir = ap "/project/dist"
        let srcDir = ap "/project/src"
        let pagesDir = ap "/project/pages"
        let srcFiles = [ap "/project/src/api.ts", ap "/project/src/types.ts"]
        let pageFiles = [ap "/project/pages/index.tsx", ap "/project/pages/about.tsx"]
        result <-
            runEff
                . runMockRoFileSystem
                    ( mockDirs
                        [ (root, [nodeModules, distDir, srcDir, pagesDir])
                        , (nodeModules, [ap "/project/node_modules/lib.ts"])
                        , (distDir, [ap "/project/dist/main.js"])
                        , (srcDir, srcFiles)
                        , (pagesDir, pageFiles)
                        ]
                    )
                $ getTsFiles root
        result `shouldBe` (srcFiles <> pageFiles)
