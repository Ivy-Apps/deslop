{-# OPTIONS_GHC -Wno-x-partial #-}

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
        it "'converts '../../' relative import to '@/'" $ do
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
            head prog'.ast
                `shouldBe` Import
                    { prefix = "import * from '"
                    , target = "@/lib/welcome"
                    , suffix = "';\n"
                    }

        it "'converts './' relative import to '@/*'" $ do
            -- Given
            let cfg = TsConfig [ImportAlias "@/" "src/"]
            let prog =
                    TsModule
                        { path = "src/features/home/home.ts"
                        , ast =
                            [ Import
                                { prefix = "import * from '"
                                , target = "./useHomeViewModel"
                                , suffix = "';\n"
                                }
                            ]
                        }

            -- When
            prog' <- runEff . runReader cfg $ importAliases prog

            -- Then
            head prog'.ast
                `shouldBe` Import
                    { prefix = "import * from '"
                    , target = "@/features/home/useHomeViewModel"
                    , suffix = "';\n"
                    }

        it "does not touch bare specifiers - e.g. 'react'" $ do
            -- Given
            let cfg = TsConfig [ImportAlias "@/" "src/"]
            let prog =
                    TsModule
                        { path = "src/features/home/home.ts"
                        , ast =
                            [ Import
                                { prefix = "import * from '"
                                , target = "react"
                                , suffix = "';\n"
                                }
                            ]
                        }

            -- When
            prog' <- runEff . runReader cfg $ importAliases prog

            -- Then
            prog `shouldBe` prog'

        it "converts more complex '@test/*' imports" $ do
            -- Given
            let cfg =
                    TsConfig
                        { paths =
                            [ ImportAlias "@test/" "tests/",
                            ImportAlias "@/" "src/"
                            ]
                        }
            let prog =
                    TsModule
                        { path = "src/features/auth.spec.ts"
                        , ast =
                            [ Import
                                { prefix = "import * from '"
                                , target = "../../tests/auth-fixture"
                                , suffix = "';\n"
                                }
                            ]
                        }

            -- When
            prog' <- runEff . runReader cfg $ importAliases prog

            -- Then
            head prog'.ast
                `shouldBe` Import
                    { prefix = "import * from '"
                    , target = "@test/auth-fixture"
                    , suffix = "';\n"
                    }