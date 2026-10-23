{-# LANGUAGE QuasiQuotes #-}

module Frontend.TypeScriptSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Module (Module (..))
import Deslop.Problem (LintRuleId (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Doubles.FileSystem (mockDirs, mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static (runReader)
import FileSystem.Path (AbsPath (..), ProjectRoot (..))
import Fixtures.Frontend.TypeScript.Config (emptyTsConfig)
import Frontend.TypeScript (TsFrontend (..))
import Generators.Frontend.TypeScript.CST (genTsProgram)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Renderable qualified
import System.OsPath (osp)
import Test.Hspec
import TestUtils (ap, prop)

spec :: Spec
spec = describe "Frontend.TypeScript" $ do
    describe "what it tells the core about itself" $ do
        it "is labelled TypeScript" $
            label TsFrontend `shouldBe` "TypeScript"

        it "fences quoted statements as ts" $
            codeFence TsFrontend `shouldBe` CodeFence "ts"

        it "names tsconfig.json as what it looks for" $
            lookingFor (Proxy @TsFrontend) `shouldBe` "TypeScript (tsconfig.json)"

        it "brings the two relative-specifier rules" $
            builtInRules TsFrontend
                `shouldBe` [LintRuleId "no-relative-imports", LintRuleId "no-relative-exports"]

    describe "detection" $ do
        it "recognises a project with a tsconfig.json at its root" $
            runEff (runMockRoFileSystem (mockFiles [[osp|/project/tsconfig.json|]]) (detect (ProjectRoot (ap "/project"))))
                `shouldReturn` Just TsFrontend

        it "does not recognise a project without one" $
            runEff (runMockRoFileSystem (mockFiles []) (detect @TsFrontend (ProjectRoot (ap "/project"))))
                `shouldReturn` Nothing

        it "does not recognise a project whose tsconfig.json is a directory" $ do
            let tsconfig = ap "/project/tsconfig.json"

            detected <-
                runEff . runMockRoFileSystem (mockDirs [(ap "/project", [tsconfig]), (tsconfig, [])]) $
                    detect @TsFrontend (ProjectRoot (ap "/project"))

            detected `shouldBe` Nothing

    describe "parsing" $ do
        -- What `deslop fix` relies on to leave every byte but a specifier alone.
        prop "renders a parsed file back to its bytes" $ do
            planted <- forAll genTsProgram
            let bytes = encodeUtf8 (Renderable.render planted)
            render TsFrontend (parse TsFrontend emptyTsConfig (ap "/project/a.ts") bytes) === bytes

        -- Total and lossless over any bytes at all, not only those of TypeScript.
        prop "renders any bytes back to themselves" $ do
            bytes <- forAll $ Gen.bytes (Range.linear 0 200)
            render TsFrontend (parse TsFrontend emptyTsConfig (ap "/project/a.ts") bytes) === bytes

    describe "a file that is not UTF-8" $ do
        let path = ap "/project/src/a.ts"
            lowered =
                runEff
                    . runConcurrent
                    . runMockRoFileSystem (mockFiles [path.osPath])
                    . runReader (ProjectRoot (ap "/project"))
                    . lower TsFrontend emptyTsConfig
                    $ [parse TsFrontend emptyTsConfig path "import x from './b';\n\xff"]

        it "is still a module, with no edges" $ do
            (ms, _) <- lowered
            fmap (.edges) ms `shouldBe` [[]]

        it "is named in a warning that its imports were not read" $ do
            (_, warnings) <- lowered
            fmap Renderable.render warnings `shouldBe` ["src/a.ts is not UTF-8, so its imports were not read."]
