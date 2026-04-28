module Deslop.RelativeImportSpec (spec) where

import Deslop.RelativeImports (importAliases)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (absPathUnsafe)
import Effects.ReportProblem (LintRuleId (..), Problem (..), getProblems, runReportProblem)
import System.OsPath (osp)
import Test.Hspec
import TestUtils (defaultTsConfig)
import TypeScript.CST (TsNode (..), TsProgram (..))

spec :: Spec
spec = describe "Deslop.RelativeImport" $ do
    let runTest cfg existingFiles prog =
            runEff
                . runMockRoFileSystem (mockFiles existingFiles)
                . runReportProblem
                . runReader cfg
                $ do
                    result <- importAliases prog
                    problems <- getProblems
                    pure (result, problems)

    let mkProg fp = TsModule (absPathUnsafe fp)
    let mkImport t = Import {prefix = "import * from '", target = t, suffix = "';\n"}

    describe "importAliases" $ do
        it "converts an up-dir relative import to an alias" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/home/home.ts|]
                        [mkImport "../../lib/welcome"]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    [[osp|/home/repo/src/lib/welcome.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@/lib/welcome"]
            length problems `shouldBe` 1
            case problems of
                (LintProblem {lintRule = r} : _) -> r `shouldBe` LintRuleId "no-relative-imports"
                _ -> expectationFailure "expected at least one problem"

        it "converts a same-dir relative import to an alias" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/home/home.ts|]
                        [mkImport "./useHomeViewModel"]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    [[osp|/home/repo/src/features/home/useHomeViewModel.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@/features/home/useHomeViewModel"]
            length problems `shouldBe` 1
            case problems of
                (LintProblem {lintRule = r} : _) -> r `shouldBe` LintRuleId "no-relative-imports"
                _ -> expectationFailure "expected at least one problem"

        it "converts a relative import crossing into the test directory" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/app.ts|]
                        [mkImport "../test/auth-fixture"]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    [[osp|/home/repo/test/auth-fixture.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@test/auth-fixture"]
            length problems `shouldBe` 1

        it "leaves a package import unchanged and reports no problem" $ do
            let prog = mkProg [osp|/home/repo/src/app.ts|] [mkImport "react"]
            (result, problems) <- runTest defaultTsConfig [] prog
            map (.target) result.cst `shouldBe` ["react"]
            problems `shouldBe` []

        it "leaves an already-aliased import unchanged and reports no problem" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/app.ts|]
                        [mkImport "@/components/Button"]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    [[osp|/home/repo/src/components/Button.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@/components/Button"]
            problems `shouldBe` []

        it "transforms multiple imports in one program" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/auth/auth.ts|]
                        [ mkImport "../../../test/auth-fixture"
                        , mkImport "react"
                        ]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    [[osp|/home/repo/test/auth-fixture.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@test/auth-fixture", "react"]
            length problems `shouldBe` 1

        it "preserves non-import nodes unchanged" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/app.ts|]
                        [ Source {raw = "const x = 1;\n"}
                        , mkImport "./utils"
                        , Source {raw = "export default x;\n"}
                        ]
            (result, _) <-
                runTest
                    defaultTsConfig
                    [[osp|/home/repo/src/utils.ts|]]
                    prog
            case result.cst of
                (n0 : _ : n2 : _) -> do
                    n0 `shouldBe` Source {raw = "const x = 1;\n"}
                    n2 `shouldBe` Source {raw = "export default x;\n"}
                _ -> expectationFailure "expected at least 3 nodes"
