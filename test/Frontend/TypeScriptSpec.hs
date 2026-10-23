{-# LANGUAGE QuasiQuotes #-}

module Frontend.TypeScriptSpec (spec) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem (LintRuleId (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Doubles.FileSystem (mockDirs, mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import FileSystem.Path (ProjectRoot (..))
import Fixtures.Frontend.TypeScript.Config (emptyTsConfig)
import Frontend.TypeScript (TsFrontend (..), TsParseError (..))
import Generators.Frontend.TypeScript.CST (genTsProgram)
import Hedgehog (forAll, (===))
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
            (render TsFrontend <$> parse TsFrontend emptyTsConfig (ap "/project/a.ts") bytes) === Right bytes

        it "logs a parse error exactly as the lexer reported it" $
            Renderable.render (TsParseError "1:8:\nunexpected end of input\n")
                `shouldBe` "1:8:\nunexpected end of input\n"
