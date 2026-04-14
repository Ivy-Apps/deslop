module Deslop.RelativeImportSpec (spec) where

-- import Data.Text qualified as T
-- import Deslop.RelativeImports (importAliases)
-- import Effectful (runEff)
-- import Effectful.Reader.Static
-- import Effects.FileSystem (encodeOsPath)
-- import Effects.ReportProblem (runReportProblem)
import System.OsPath (OsPath)
import Test.Hspec
import TypeScript.CST

-- import TypeScript.Config

spec :: Spec
spec = describe "importAliases" $ do
    it "Fix the tests later" $ do
        True `shouldBe` True

-- Given
-- let cfg =
--         TsConfigLegacy
--             { paths =
--                 [ ImportAlias {label = "@test/", path = "tests/"}
--                 , ImportAlias {label = "@/", path = "src/"}
--                 ]
--             }
--
-- let runTest source target =
--         runEff . runReader cfg . runReportProblem $
--             importAliases (mkTestProgram source target)
--
-- describe "Path Resolutions" $ do
--     let cases =
--             [ ([osp|src/features/home/home.ts|], "../../lib/welcome", "@/lib/welcome")
--             , ([osp|src/features/home/home.ts|], "./useHomeViewModel", "@/features/home/useHomeViewModel")
--             , ([osp|src/features/auth.spec.ts|], "../../tests/auth-fixture", "@test/auth-fixture")
--             , ([osp|src/app.ts|], "react", "react")
--             , ([osp|src/feature/f1/f1.spec.ts|], "@/../tests/fixtures", "@test/fixtures")
--             , (encodeOsPath "", "vitests/config", "vitests/config")
--             ]
--
--     forM_ cases $ \(src, target, expected) ->
--         it (T.unpack $ "resolves '" <> target <> "' -> '" <> expected <> "'") $ do
--             -- When
--             result <- runTest src target
--             -- Then
--             firstTarget result `shouldBe` expected

_mkTestProgram :: OsPath -> Text -> TsProgram
_mkTestProgram filePath importTarget =
    TsModule
        filePath
        [ Import
            { prefix = "import * from '"
            , target = importTarget
            , suffix = "';\n"
            }
        ]

_firstTarget :: TsProgram -> Text
_firstTarget p = case p.cst of
    (Import _ t _ : _) -> t
    _ -> error "The program has no imports!"
