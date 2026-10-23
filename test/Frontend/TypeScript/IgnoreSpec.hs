module Frontend.TypeScript.IgnoreSpec (spec) where

import Data.Set qualified as Set
import Doubles.FileSystem (MockRoFileSystem (..), mockDirs, runMockRoFileSystem)
import Effectful (runEff)
import FileSystem.Iterator (Entry (..))
import FileSystem.Path (AbsPath)
import Frontend.TypeScript.Ignore (jsEcosystemIgnored, jsEcosystemIgnoredNames)
import Frontend.TypeScript.Iterator (getTsFiles)
import Git.Ignore (GitIgnore (..), IgnoreScope (..), alwaysIgnoredNames, emptyGitIgnore, loadGitIgnore, parseIgnoreFile)
import Test.Hspec
import TestUtils (ap)

root :: AbsPath
root = ap "/project"

spec :: Spec
spec = describe "Frontend.TypeScript.Ignore" $ do
    describe "always-ignored directories" $ do
        it "ignores node_modules regardless of any rule" $ do
            let nodeModules = ap "/project/node_modules"
            let reIncluding = GitIgnore {root, ecosystem = jsEcosystemIgnored, scopes = [IgnoreScope {base = root, rules = parseIgnoreFile "!node_modules"}]}

            files <-
                runEff . runMockRoFileSystem (mockDirs [(root, [nodeModules]), (nodeModules, [ap "/project/node_modules/dep.ts"])]) $
                    getTsFiles reIncluding root

            files `shouldBe` []

        it "ignores files beneath an always-ignored directory" $ do
            let nodeModules = ap "/project/node_modules"
            let pkg = ap "/project/node_modules/pkg"

            files <-
                runEff . runMockRoFileSystem (mockDirs [(root, [nodeModules]), (nodeModules, [pkg]), (pkg, [ap "/project/node_modules/pkg/index.ts"])]) $
                    getTsFiles (emptyGitIgnore jsEcosystemIgnored root) root

            files `shouldBe` []

        it "matches an always-ignored name exactly, not as a prefix" $
            jsEcosystemIgnored Entry {path = ap "/project/node_modules_backup", isDir = True} `shouldBe` False

        it "adds only names the core does not already ignore" $
            Set.fromList jsEcosystemIgnoredNames `Set.intersection` Set.fromList alwaysIgnoredNames
                `shouldBe` Set.empty

    describe "loading .gitignore files" $
        -- Dependencies are the largest tree in a project; walking them for
        -- .gitignore files costs more than the rest of the run combined.
        it "never walks into node_modules looking for a .gitignore" $ do
            let nodeModules = ap "/project/node_modules"
            let fs =
                    (mockDirs [(root, [nodeModules]), (nodeModules, [ap "/project/node_modules/.gitignore"])])
                        { mockReadFile = const . pure $ "*.ts"
                        }

            gitIgnore <- runEff . runMockRoFileSystem fs $ loadGitIgnore jsEcosystemIgnored root

            gitIgnore.scopes `shouldBe` []
