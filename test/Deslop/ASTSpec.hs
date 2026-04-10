module Deslop.ASTSpec (spec) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    ModuleId (..),
    parseAst,
 )
import Effectful
import Effectful.Reader.Static (runReader)
import System.OsPath (osp)
import Test.Hspec
import TestUtils (defaultTsConfig)
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Config

spec :: Spec
spec = describe "parseAst" $ do
    it "simple happy path" $ do
        let prog =
                TsModule
                    { path = [osp|src/lib/demo.ts|]
                    , cst =
                        [ Import
                            { prefix = "import * from'"
                            , target = "@/types/errors"
                            , suffix = "';"
                            }
                        ]
                    }
        ast <- runEff . runReader @TsConfig defaultTsConfig $ parseAst prog
        ast
            `shouldBe` AstModule
                { id = ModuleId "@/lib/demo"
                , nodes =
                    [ ImportNode {target = ModuleId "@/types/errors"}
                    ]
                }

    it "import alias not available" $ do
        let prog =
                TsModule
                    { path = [osp|src/main.ts|]
                    , cst =
                        [ Import
                            { prefix = "import { useEffect } from '"
                            , target = "react"
                            , suffix = "';\n"
                            }
                        , Import
                            { prefix = "import type { Error } from '"
                            , target = "src/types/errors"
                            , suffix = "';"
                            }
                        ]
                    }
        ast <- runEff . runReader @TsConfig (TsConfig []) $ parseAst prog
        ast
            `shouldBe` AstModule
                { id = ModuleId "src/main"
                , nodes =
                    [ ImportNode {target = ModuleId "react"}
                    , ImportNode {target = ModuleId "src/types/errors"}
                    ]
                }
