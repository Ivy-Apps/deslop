module TypeScript.IteratorSpec (spec) where

import Doubles.FileSystem (mockDirs, runMockRoFileSystem)
import Effectful (runEff)
import Effects.FileSystem (AbsPath)
import Test.Hspec
import TestUtils (ap)
import TypeScript.Iterator (getTsFiles)

root :: AbsPath
root = ap "/project"

-- | Run getTsFiles against a mock filesystem rooted at '/project'.
run :: [(AbsPath, [AbsPath])] -> IO [AbsPath]
run dirs = runEff . runMockRoFileSystem (mockDirs dirs) $ getTsFiles root

spec :: Spec
spec = describe "TypeScript.Iterator" $ do
    it "returns empty list for empty directory" $
        run [(root, [])] >>= (`shouldBe` [])

    it "collects multiple .ts and .tsx files from a flat directory" $ do
        let files =
                [ ap "/project/index.ts"
                , ap "/project/App.tsx"
                , ap "/project/utils.ts"
                , ap "/project/Button.tsx"
                ]
        run [(root, files)] >>= (`shouldBe` files)

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
        run [(root, allEntries)] >>= (`shouldBe` [tsFile, tsxFile])

    it "recurses into a subdirectory and collects .ts and .tsx files" $ do
        let srcDir = ap "/project/src"
        let files =
                [ ap "/project/src/main.ts"
                , ap "/project/src/App.tsx"
                , ap "/project/src/index.ts"
                ]
        run [(root, [srcDir]), (srcDir, files)] >>= (`shouldBe` files)

    it "recurses deeply (3 levels) and collects all .ts/.tsx files" $ do
        let componentsDir = ap "/project/src/components"
        let srcDir = ap "/project/src"
        let rootTs = ap "/project/root.ts"
        let srcTs = ap "/project/src/service.ts"
        let compTs = ap "/project/src/components/Button.tsx"
        let compTs2 = ap "/project/src/components/Modal.ts"
        run
            [ (root, [rootTs, srcDir])
            , (srcDir, [srcTs, componentsDir])
            , (componentsDir, [compTs, compTs2])
            ]
            >>= (`shouldBe` [rootTs, srcTs, compTs, compTs2])

    it "skips an empty non-ignored subdirectory" $ do
        let emptyDir = ap "/project/empty"
        run [(root, [emptyDir]), (emptyDir, [])] >>= (`shouldBe` [])

    describe "ignored directories" $ do
        it "skips node_modules at root" $ do
            let nodeModules = ap "/project/node_modules"
            let srcFile = ap "/project/index.ts"
            run
                [ (root, [nodeModules, srcFile])
                , (nodeModules, [ap "/project/node_modules/dep.ts"])
                ]
                >>= (`shouldBe` [srcFile])

        it "skips node_modules nested inside a subdirectory" $ do
            let pkgDir = ap "/project/packages/app"
            let nestedNm = ap "/project/packages/app/node_modules"
            let appTs = ap "/project/packages/app/index.ts"
            let packagesDir = ap "/project/packages"
            run
                [ (root, [packagesDir])
                , (packagesDir, [pkgDir])
                , (pkgDir, [nestedNm, appTs])
                , (nestedNm, [ap "/project/packages/app/node_modules/lib.ts"])
                ]
                >>= (`shouldBe` [appTs])

        it "skips node_modules at multiple nesting levels simultaneously" $ do
            let rootNm = ap "/project/node_modules"
            let srcDir = ap "/project/src"
            let nestedNm = ap "/project/src/node_modules"
            let srcTs = ap "/project/src/api.ts"
            run
                [ (root, [rootNm, srcDir])
                , (rootNm, [ap "/project/node_modules/pkg.ts"])
                , (srcDir, [nestedNm, srcTs])
                , (nestedNm, [ap "/project/src/node_modules/dep.ts"])
                ]
                >>= (`shouldBe` [srcTs])

        it "does NOT skip a directory named node_modules_backup (exact name match)" $ do
            let nmBackup = ap "/project/node_modules_backup"
            let backupTs = ap "/project/node_modules_backup/legacy.ts"
            run
                [(root, [nmBackup]), (nmBackup, [backupTs])]
                >>= (`shouldBe` [backupTs])

        it "skips .git" $ do
            let gitDir = ap "/project/.git"
            run
                [(root, [gitDir]), (gitDir, [ap "/project/.git/hook.ts"])]
                >>= (`shouldBe` [])

        it "skips dist" $ do
            let distDir = ap "/project/dist"
            run
                [ (root, [distDir])
                , (distDir, [ap "/project/dist/bundle.ts", ap "/project/dist/chunk.tsx"])
                ]
                >>= (`shouldBe` [])

        it "skips .next" $ do
            let nextDir = ap "/project/.next"
            run
                [(root, [nextDir]), (nextDir, [ap "/project/.next/server.ts"])]
                >>= (`shouldBe` [])

        it "skips all ignored dirs simultaneously while collecting from valid ones" $ do
            let nodeModules = ap "/project/node_modules"
            let distDir = ap "/project/dist"
            let gitDir = ap "/project/.git"
            let nextDir = ap "/project/.next"
            let srcDir = ap "/project/src"
            let pagesDir = ap "/project/pages"
            let srcFiles = [ap "/project/src/api.ts", ap "/project/src/types.ts"]
            let pageFiles = [ap "/project/pages/index.tsx", ap "/project/pages/about.tsx"]
            run
                [ (root, [nodeModules, distDir, gitDir, nextDir, srcDir, pagesDir])
                , (nodeModules, [ap "/project/node_modules/lib.ts"])
                , (distDir, [ap "/project/dist/main.js"])
                , (gitDir, [ap "/project/.git/config.ts"])
                , (nextDir, [ap "/project/.next/server.ts"])
                , (srcDir, srcFiles)
                , (pagesDir, pageFiles)
                ]
                >>= (`shouldBe` (srcFiles <> pageFiles))
