module Deslop.RelativeImportSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Deslop.RelativeImports (importAliases)
import Effectful (runEff)
import Effectful.Reader.Static
import Effects.ReportProblem (runReportProblem)
import Test.Hspec
import TypeScript.AST
import TypeScript.Config

spec :: Spec
spec = describe "importAliases" $ do
    -- Given
    let cfg =
            TsConfig
                { paths =
                    [ ImportAlias {label = "@test/", path = "tests/"}
                    , ImportAlias {label = "@/", path = "src/"}
                    ]
                }

    let runTest source target =
            runEff . runReader cfg . runReportProblem $
                importAliases (mkTestProgram source target)

    describe "Happy Path Resolutions" $ do
        let cases =
                [ ("src/features/home/home.ts", "../../lib/welcome", "@/lib/welcome")
                , ("src/features/home/home.ts", "./useHomeViewModel", "@/features/home/useHomeViewModel")
                , ("src/features/auth.spec.ts", "../../tests/auth-fixture", "@test/auth-fixture")
                , ("src/app.ts", "react", "react")
                , ("src/feature/f1/f1.spec.ts", "@/../tests/fixtures", "@test/fixtures")
                , ("", "vitests/config", "vitests/config")
                ]

        forM_ cases $ \(src, target, expected) ->
            it (T.unpack $ "resolves '" <> target <> "' -> '" <> expected <> "'") $ do
                -- When
                result <- runTest src target
                -- Then
                firstTarget result `shouldBe` expected

mkTestProgram :: FilePath -> Text -> TsProgram
mkTestProgram filePath importTarget =
    TsModule
        filePath
        [ Import
            { prefix = "import * from '"
            , target = importTarget
            , suffix = "';\n"
            }
        ]

firstTarget :: TsProgram -> Text
firstTarget p = case p.ast of
    (Import _ t _ : _) -> t
    _ -> error "The program has no imports!"
