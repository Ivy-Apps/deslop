module Deslop.ImportSpec where

import Deslop.Imports (importAliases)
import Effectful (runEff)
import Effectful.Reader.Static
import Test.Hspec
import TypeScript.AST
import TypeScript.Config

spec :: Spec
spec = do
    describe "Happy path" $ do
        it "'Convers relative import to '@/'" $ do
            -- Given
            let cfg = TsConfig [ImportAlias "@/" "src/"]
            let prog =
                    TsModule
                        { path = "src/features/home/home.ts"
                        , ast =
                            [ Import
                                { prefix = "import * from '"
                                , target = "../../lib/welcome"
                                , suffix = "';\n"
                                }
                            ]
                        }

            -- When
            prog' <- runEff . runReader cfg $ importAliases prog

            -- Then
            prog.path `shouldBe` prog'.path
            head prog'.ast
                `shouldBe` Import
                    { prefix = "import * from '"
                    , target = "@/lib/welcome"
                    , suffix = "';\n"
                    }
