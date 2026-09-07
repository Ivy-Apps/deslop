module TypeScript.Lint.RelativeSpecifiersSpec (spec) where

import Data.Text qualified as T
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..), ProblemId (..), problemId)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import FileSystem.Path (ProjectRoot (..), RelativePath (..), absPathUnsafe, decodeOsPath, encodeOsPath)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Fixtures.TypeScript.Config (defaultTsConfig, mkMapping)
import Hedgehog (Gen, evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (osp)
import Test.Hspec
import TestUtils (ap, prop)
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Config (Pattern (..), TsConfig (..))
import TypeScript.Lint.RelativeSpecifiers (noRelativeSpecifiers)

repoRoot :: ProjectRoot
repoRoot = ProjectRoot (absPathUnsafe [osp|/home/repo|])

spec :: Spec
spec = describe "TypeScript.Lint.RelativeSpecifiers" $ do
    let runTestIn root cfg baseline existingFiles prog =
            runEff
                . runMockRoFileSystem (mockFiles existingFiles)
                . runReportProblem
                . runReader @ProjectRoot root
                . runReader cfg
                . runReader baseline
                $ do
                    result <- noRelativeSpecifiers prog
                    problems <- getProblems
                    pure (result, problems)

    let runTest = runTestIn repoRoot
    let runTestNoBaseline cfg = runTest cfg (baselineOf [])

    let mkProg fp = TsModule (absPathUnsafe fp)
    let mkImport t = Import {prefix = "import * from '", target = t, suffix = "';\n"}
    let mkReExport t = ReExport {prefix = "export * from '", target = t, suffix = "';\n"}

    describe "noRelativeSpecifiers" $ do
        it "converts an up-dir relative import to an alias" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/home/home.ts|]
                        [mkImport "../../lib/welcome"]
            (result, problems) <-
                runTestNoBaseline
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
                runTestNoBaseline
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
                runTestNoBaseline
                    defaultTsConfig
                    [[osp|/home/repo/test/auth-fixture.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@test/auth-fixture"]
            length problems `shouldBe` 1

        it "leaves a package import unchanged and reports no problem" $ do
            let prog = mkProg [osp|/home/repo/src/app.ts|] [mkImport "react"]
            (result, problems) <- runTestNoBaseline defaultTsConfig [] prog
            map (.target) result.cst `shouldBe` ["react"]
            problems `shouldBe` []

        it "leaves an already-aliased import unchanged and reports no problem" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/app.ts|]
                        [mkImport "@/components/Button"]
            (result, problems) <-
                runTestNoBaseline
                    defaultTsConfig
                    [[osp|/home/repo/src/components/Button.ts|]]
                    prog
            map (.target) result.cst `shouldBe` ["@/components/Button"]
            problems `shouldBe` []

        it "converts a relative re-export to an alias, under its own rule" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/lib/index.ts|]
                        [mkReExport "./welcome"]
            (result, problems) <-
                runTestNoBaseline defaultTsConfig [[osp|/home/repo/src/lib/welcome.ts|]] prog

            map (.target) result.cst `shouldBe` ["@/lib/welcome"]
            case problems of
                [LintProblem {lintRule = r}] -> r `shouldBe` LintRuleId "no-relative-exports"
                _ -> expectationFailure "expected exactly one no-relative-exports problem"

        it "leaves an already-aliased re-export unchanged and reports no problem" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/lib/index.ts|]
                        [mkReExport "@/lib/welcome"]
            (result, problems) <-
                runTestNoBaseline defaultTsConfig [[osp|/home/repo/src/lib/welcome.ts|]] prog

            map (.target) result.cst `shouldBe` ["@/lib/welcome"]
            problems `shouldBe` []

        -- One module answers to several names, so a written alias that is not
        -- the canonical one is that module named another way, not a relative
        -- import. Reporting it said "Relative imports are not allowed" about an
        -- import containing nothing relative.
        it "leaves a non-canonical alias alone rather than calling it relative" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/app.ts|]
                        [mkImport "@test/welcome"]
            (result, problems) <-
                runTestNoBaseline defaultTsConfig [[osp|/home/repo/test/welcome.ts|]] prog

            map (.target) result.cst `shouldBe` ["@test/welcome"]
            problems `shouldBe` []

        -- The two rules have separate ids so that accepting one does not
        -- silence the other, which a shared id would do: a Lint Problem's id is
        -- {rule}#{file}, and these two share a file.
        it "reports an import and a re-export in one file under separate ids" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/lib/index.ts|]
                        [mkImport "./welcome", mkReExport "./welcome"]
            (_, problems) <-
                runTestNoBaseline defaultTsConfig [[osp|/home/repo/src/lib/welcome.ts|]] prog

            sort (map problemId problems)
                `shouldBe` sort
                    [ ProblemId "no-relative-imports#src/lib/index.ts"
                    , ProblemId "no-relative-exports#src/lib/index.ts"
                    ]

        it "baselining the import does not silence the re-export" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/lib/index.ts|]
                        [mkImport "./welcome", mkReExport "./welcome"]
            let baseline = baselineOf ["no-relative-imports#src/lib/index.ts"]
            (result, _) <-
                runTest defaultTsConfig baseline [[osp|/home/repo/src/lib/welcome.ts|]] prog

            -- The baselined import keeps its original text; the re-export is
            -- still fixed.
            map (.target) result.cst `shouldBe` ["./welcome", "@/lib/welcome"]

        it "transforms multiple imports in one program" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/auth/auth.ts|]
                        [ mkImport "../../../test/auth-fixture"
                        , mkImport "react"
                        ]
            (result, problems) <-
                runTestNoBaseline
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
                runTestNoBaseline
                    defaultTsConfig
                    [[osp|/home/repo/src/utils.ts|]]
                    prog
            case result.cst of
                (n0 : _ : n2 : _) -> do
                    n0 `shouldBe` Source {raw = "const x = 1;\n"}
                    n2 `shouldBe` Source {raw = "export default x;\n"}
                _ -> expectationFailure "expected at least 3 nodes"

    describe "noRelativeSpecifiers with baseline" $ do
        it "baselined import is reported as problem but kept as-is (not fixed)" $ do
            -- problemId for LintProblem = lintRuleId <> "#" <> filePath
            -- file is relative to the project root, so:
            -- "no-relative-imports#src/features/home/home.ts"
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/home/home.ts|]
                        [mkImport "../../lib/welcome"]
            let baseline = baselineOf ["no-relative-imports#src/features/home/home.ts"]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    baseline
                    [[osp|/home/repo/src/lib/welcome.ts|]]
                    prog
            -- import is NOT fixed — kept as original relative import
            map (.target) result.cst `shouldBe` ["../../lib/welcome"]
            -- problem is still reported
            length problems `shouldBe` 1

        it "import in a different (non-baselined) file is still fixed" $ do
            -- baseline only covers home.ts, not auth.ts
            let prog =
                    mkProg
                        [osp|/home/repo/src/features/auth/auth.ts|]
                        [mkImport "../../../test/auth-fixture"]
            let baseline = baselineOf ["no-relative-imports#src/features/home/home.ts"]
            (result, problems) <-
                runTest
                    defaultTsConfig
                    baseline
                    [[osp|/home/repo/test/auth-fixture.ts|]]
                    prog
            -- auth.ts is not baselined, so the import is fixed
            map (.target) result.cst `shouldBe` ["@test/auth-fixture"]
            length problems `shouldBe` 1

    describe "the reported location" $ do
        -- The mono-repo layout this rule is most often wrong in: no `baseUrl`
        -- anywhere, so the paths base is the directory of the config that
        -- declared `paths` - `config/`, which holds no sources at all.
        let monorepoConfig =
                TsConfig
                    { pathsBase = ap "/home/repo/config"
                    , paths = [mkMapping (Wildcard "@app/" "") [Wildcard "../packages/app/src/" ""]]
                    }

        it "is relative to the project root, not to the paths base" $ do
            let prog =
                    mkProg
                        [osp|/home/repo/packages/app/src/checkout.ts|]
                        [mkImport "./helpers/format"]

            (_, problems) <-
                runTestNoBaseline
                    monorepoConfig
                    [[osp|/home/repo/packages/app/src/helpers/format.ts|]]
                    prog

            map locationOf problems `shouldBe` ["packages/app/src/checkout.ts"]

        prop "is relative to the project root, wherever the paths base is" $ do
            rootSegs <- forAll (genSegments "r" 1 3)
            dirSegs <- forAll (genSegments "d" 1 3)
            pathsBase <- forAll (genPathsBase rootSegs dirSegs)

            let dir = joinAbs (rootSegs <> dirSegs)
                cfg =
                    TsConfig
                        { pathsBase = ap pathsBase
                        , paths = [mkMapping (Wildcard "@/" "") [Wildcard "" ""]]
                        }
                prog = mkProg (encodeOsPath (dir <> "/main.ts")) [mkImport "./helper"]

            (_, problems) <-
                evalIO $
                    runTestIn
                        (ProjectRoot (ap (joinAbs rootSegs)))
                        cfg
                        (baselineOf [])
                        [encodeOsPath (dir <> "/helper.ts")]
                        prog

            map locationOf problems === [T.intercalate "/" (dirSegs <> ["main.ts"])]

locationOf :: Problem -> Text
locationOf p = case p of
    LintProblem {location = Location {file}} -> decodeOsPath file.osPath
    RuleViolation {} -> "not a lint problem"

joinAbs :: [Text] -> Text
joinAbs = ("/" <>) . T.intercalate "/"

{- | Segments tagged by which part of the path they belong to, so a generated
root and module directory never share a segment by coincidence.
-}
genSegments :: Text -> Int -> Int -> Gen [Text]
genSegments tag lo hi = do
    n <- Gen.int (Range.linear lo hi)
    pure [tag <> show i | i <- [1 .. n]]

{- | Every shape a paths base takes in the wild: the project root itself (a
declared @baseUrl@ of @"."@), a config directory holding no sources, the module
directory, and a directory outside the project entirely.
-}
genPathsBase :: [Text] -> [Text] -> Gen Text
genPathsBase rootSegs dirSegs =
    Gen.element
        [ joinAbs rootSegs
        , joinAbs (rootSegs <> ["config"])
        , joinAbs (rootSegs <> dirSegs)
        , "/elsewhere"
        ]
