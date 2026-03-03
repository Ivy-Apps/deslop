module Deslop.ASTSpec (spec) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    ModuleId (..),
    parseAst,
 )
import Effectful
import Effectful.Reader.Static (runReader)
import Test.Hspec
import TestUtils (defaultTsConfig)
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Config

spec :: Spec
spec = describe "parseAst" $
    it "simple happy path" $ do
        let prog =
                TsModule
                    { path = "src/lib/demo.ts"
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
