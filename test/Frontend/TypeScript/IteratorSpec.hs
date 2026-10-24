module Frontend.TypeScript.IteratorSpec (spec) where

import Doubles.FileSystem (mockDirs, runMockRoFileSystem)
import Effectful (runEff)
import FileSystem.Path (AbsPath, ProjectRoot (..))
import Fixtures.FileSystem.Path (inProject, projectRoot)
import Fixtures.Git.Ignore qualified as Fixture
import Frontend.TypeScript.Ignore (jsEcosystemIgnored)
import Frontend.TypeScript.Iterator (getTsFiles)
import Git.Ignore (GitIgnore, emptyGitIgnore)
import Test.Hspec

root :: AbsPath
root = projectRoot.path

-- | Run getTsFiles against a mock filesystem rooted at '/project'.
run :: [(AbsPath, [AbsPath])] -> IO [AbsPath]
run = runWith (emptyGitIgnore jsEcosystemIgnored root)

runWith :: GitIgnore -> [(AbsPath, [AbsPath])] -> IO [AbsPath]
runWith gitIgnore dirs = runEff . runMockRoFileSystem (mockDirs dirs) $ getTsFiles gitIgnore root

-- | A GitIgnore holding a single root .gitignore with the given contents.
rootGitIgnore :: Text -> GitIgnore
rootGitIgnore = Fixture.rootGitIgnore jsEcosystemIgnored root

spec :: Spec
spec = describe "Frontend.TypeScript.Iterator" $ do
    it "returns empty list for empty directory" $
        run [(root, [])] >>= (`shouldBe` [])

    it "collects multiple .ts and .tsx files from a flat directory" $ do
        let files =
                [ inProject "index.ts"
                , inProject "App.tsx"
                , inProject "utils.ts"
                , inProject "Button.tsx"
                ]
        run [(root, files)] >>= (`shouldBe` files)

    it "excludes non-TypeScript files alongside .ts/.tsx files" $ do
        let tsFile = inProject "index.ts"
        let tsxFile = inProject "App.tsx"
        let allEntries =
                [ tsFile
                , inProject "styles.css"
                , tsxFile
                , inProject "README.md"
                , inProject "build.js"
                ]
        run [(root, allEntries)] >>= (`shouldBe` [tsFile, tsxFile])

    it "recurses into a subdirectory and collects .ts and .tsx files" $ do
        let srcDir = inProject "src"
        let files =
                [ inProject "src/main.ts"
                , inProject "src/App.tsx"
                , inProject "src/index.ts"
                ]
        run [(root, [srcDir]), (srcDir, files)] >>= (`shouldBe` files)

    it "recurses deeply (3 levels) and collects all .ts/.tsx files" $ do
        let componentsDir = inProject "src/components"
        let srcDir = inProject "src"
        let rootTs = inProject "root.ts"
        let srcTs = inProject "src/service.ts"
        let compTs = inProject "src/components/Button.tsx"
        let compTs2 = inProject "src/components/Modal.ts"
        run
            [ (root, [rootTs, srcDir])
            , (srcDir, [srcTs, componentsDir])
            , (componentsDir, [compTs, compTs2])
            ]
            >>= (`shouldBe` [rootTs, srcTs, compTs, compTs2])

    it "skips an empty non-ignored subdirectory" $ do
        let emptyDir = inProject "empty"
        run [(root, [emptyDir]), (emptyDir, [])] >>= (`shouldBe` [])

    describe "ignored directories" $ do
        it "skips node_modules at root" $ do
            let nodeModules = inProject "node_modules"
            let srcFile = inProject "index.ts"
            run
                [ (root, [nodeModules, srcFile])
                , (nodeModules, [inProject "node_modules/dep.ts"])
                ]
                >>= (`shouldBe` [srcFile])

        it "skips node_modules nested inside a subdirectory" $ do
            let pkgDir = inProject "packages/app"
            let nestedNm = inProject "packages/app/node_modules"
            let appTs = inProject "packages/app/index.ts"
            let packagesDir = inProject "packages"
            run
                [ (root, [packagesDir])
                , (packagesDir, [pkgDir])
                , (pkgDir, [nestedNm, appTs])
                , (nestedNm, [inProject "packages/app/node_modules/lib.ts"])
                ]
                >>= (`shouldBe` [appTs])

        it "skips node_modules at multiple nesting levels simultaneously" $ do
            let rootNm = inProject "node_modules"
            let srcDir = inProject "src"
            let nestedNm = inProject "src/node_modules"
            let srcTs = inProject "src/api.ts"
            run
                [ (root, [rootNm, srcDir])
                , (rootNm, [inProject "node_modules/pkg.ts"])
                , (srcDir, [nestedNm, srcTs])
                , (nestedNm, [inProject "src/node_modules/dep.ts"])
                ]
                >>= (`shouldBe` [srcTs])

        it "does NOT skip a directory named node_modules_backup (exact name match)" $ do
            let nmBackup = inProject "node_modules_backup"
            let backupTs = inProject "node_modules_backup/legacy.ts"
            run
                [(root, [nmBackup]), (nmBackup, [backupTs])]
                >>= (`shouldBe` [backupTs])

        it "skips .git" $ do
            let gitDir = inProject ".git"
            run
                [(root, [gitDir]), (gitDir, [inProject ".git/hook.ts"])]
                >>= (`shouldBe` [])

        it "skips dist" $ do
            let distDir = inProject "dist"
            run
                [ (root, [distDir])
                , (distDir, [inProject "dist/bundle.ts", inProject "dist/chunk.tsx"])
                ]
                >>= (`shouldBe` [])

        it "skips .next" $ do
            let nextDir = inProject ".next"
            run
                [(root, [nextDir]), (nextDir, [inProject ".next/server.ts"])]
                >>= (`shouldBe` [])

        it "skips next-env.d.ts" $ do
            let nextTypes = inProject "next-env.d.ts"
            run [(root, [nextTypes])] >>= (`shouldBe` [])

        it "skips .direnv" $ do
            let direnvDir = inProject ".direnv"
            run
                [(root, [direnvDir]), (direnvDir, [inProject ".direnv/flake-inputs/src/api.ts"])]
                >>= (`shouldBe` [])

        it "skips AI agent folders, worktrees included" $ do
            let agentDirs = [".claude", ".codex", ".cursor", ".opencode", ".windsurf"]
            run
                ((root, inProject <$> agentDirs) : fmap (\dir -> (inProject dir, [inProject $ dir <> "/worktrees/x/src/api.ts"])) agentDirs)
                >>= (`shouldBe` [])

        it "skips all ignored dirs simultaneously while collecting from valid ones" $ do
            let nodeModules = inProject "node_modules"
            let distDir = inProject "dist"
            let gitDir = inProject ".git"
            let nextDir = inProject ".next"
            let srcDir = inProject "src"
            let pagesDir = inProject "pages"
            let srcFiles = [inProject "src/api.ts", inProject "src/types.ts"]
            let pageFiles = [inProject "pages/index.tsx", inProject "pages/about.tsx"]
            run
                [
                    ( root
                    ,
                        [ nodeModules
                        , distDir
                        , gitDir
                        , nextDir
                        , srcDir
                        , pagesDir
                        , inProject ".next-env.d.ts"
                        ]
                    )
                , (nodeModules, [inProject "node_modules/lib.ts"])
                , (distDir, [inProject "dist/main.js"])
                , (gitDir, [inProject ".git/config.ts"])
                , (nextDir, [inProject ".next/server.ts"])
                , (srcDir, srcFiles)
                , (pagesDir, pageFiles)
                ]
                >>= (`shouldBe` (srcFiles <> pageFiles))

    describe "gitignored paths" $ do
        it "skips a gitignored file" $ do
            let keep = inProject "keep.ts"
            runWith
                (rootGitIgnore "generated.ts")
                [(root, [keep, inProject "generated.ts"])]
                >>= (`shouldBe` [keep])

        it "skips a gitignored directory without descending into it" $ do
            let vendorDir = inProject "vendor"
            let keep = inProject "keep.ts"
            runWith
                (rootGitIgnore "vendor/")
                [(root, [vendorDir, keep]), (vendorDir, [inProject "vendor/legacy.ts"])]
                >>= (`shouldBe` [keep])

        it "keeps a file re-included by a later negation" $ do
            let keep = inProject "keep.gen.ts"
            runWith
                (rootGitIgnore "*.gen.ts\n!keep.gen.ts")
                [(root, [keep, inProject "other.gen.ts"])]
                >>= (`shouldBe` [keep])

        it "still skips the hardcoded list even when a rule un-ignores it" $ do
            let nodeModules = inProject "node_modules"
            let keep = inProject "keep.ts"
            runWith
                (rootGitIgnore "!node_modules")
                [(root, [nodeModules, keep]), (nodeModules, [inProject "node_modules/dep.ts"])]
                >>= (`shouldBe` [keep])
