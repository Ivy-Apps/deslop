{-# OPTIONS_GHC -Wno-x-partial #-}

module Deslop.ImportSpec where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Deslop.Imports (importAliases)
import Effectful (runEff)
import Effectful.Reader.Static
import Test.Hspec
import TypeScript.AST
import TypeScript.Config

spec :: Spec
spec = describe "importAliases" $ do
    -- Given
    let cfg =
            TsConfig
                { paths =
                    [ ImportAlias "@/" "src/"
                    , ImportAlias "@test/" "tests/"
                    ]
                }

    let runTest source target =
            runEff . runReader cfg $
                importAliases (mkTestProgram source target)

    describe "Happy Path Resolutions" $ do
        let cases =
                [ ("src/features/home/home.ts", "../../lib/welcome", "@/lib/welcome")
                , ("src/features/home/home.ts", "./useHomeViewModel", "@/features/home/useHomeViewModel")
                , ("src/features/auth.spec.ts", "../../tests/auth-fixture", "@test/auth-fixture")
                , ("src/app.ts", "react", "react")
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