module Frontend.TypeScript.IgnoreSpec (spec) where

import Data.Set qualified as Set
import Doubles.FileSystem (MockRoFileSystem (..), mockDirs, runMockRoFileSystem)
import Effectful (runEff)
import FileSystem.Iterator (Entry (..))
import FileSystem.Path (AbsPath, ProjectRoot (..))
import Fixtures.FileSystem.Path (inProject, projectRoot)
import Fixtures.Git.Ignore (rootGitIgnore)
import Frontend.TypeScript.Ignore (jsEcosystemIgnored, jsEcosystemIgnoredNames)
import Frontend.TypeScript.Iterator (getTsFiles)
import Git.Ignore (alwaysIgnoredNames, emptyGitIgnore, isIgnored, loadGitIgnore)
import Test.Hspec

root :: AbsPath
root = projectRoot.path

spec :: Spec
spec = describe "Frontend.TypeScript.Ignore" $ do
    describe "always-ignored directories" $ do
        it "ignores node_modules regardless of any rule" $ do
            let nodeModules = inProject "node_modules"
            let reIncluding = rootGitIgnore jsEcosystemIgnored root "!node_modules"

            files <-
                runEff . runMockRoFileSystem (mockDirs [(root, [nodeModules]), (nodeModules, [inProject "node_modules/dep.ts"])]) $
                    getTsFiles reIncluding root

            files `shouldBe` []

        it "ignores files beneath an always-ignored directory" $ do
            let nodeModules = inProject "node_modules"
            let pkg = inProject "node_modules/pkg"

            files <-
                runEff . runMockRoFileSystem (mockDirs [(root, [nodeModules]), (nodeModules, [pkg]), (pkg, [inProject "node_modules/pkg/index.ts"])]) $
                    getTsFiles (emptyGitIgnore jsEcosystemIgnored root) root

            files `shouldBe` []

        it "matches an always-ignored name exactly, not as a prefix" $
            jsEcosystemIgnored Entry {path = inProject "node_modules_backup", isDir = True} `shouldBe` False

        it "adds only names the core does not already ignore" $
            Set.fromList jsEcosystemIgnoredNames `Set.intersection` Set.fromList alwaysIgnoredNames
                `shouldBe` Set.empty

    describe "loading .gitignore files" $
        -- Dependencies are the largest tree in a project; walking them for
        -- .gitignore files costs more than the rest of the run combined.
        it "never walks into node_modules looking for a .gitignore" $ do
            let nodeModules = inProject "node_modules"
                listing dir
                    | dir == nodeModules = error "walked into node_modules"
                    | otherwise = pure [nodeModules]
                fs =
                    (mockDirs [(root, [nodeModules])])
                        { mockListDirectory = listing
                        }

            gitIgnore <- runEff . runMockRoFileSystem fs $ loadGitIgnore jsEcosystemIgnored root

            isIgnored gitIgnore Entry {path = inProject "src/a.ts", isDir = False} `shouldBe` False
