module Deslop.ASTSpec (spec) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    parseAst,
 )
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (runFileSystemIO)
import System.OsPath (osp)
import Test.Hspec
import TestUtils (defaultTsConfig, emptyTsConfig)
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Config
import TypeScript.ModuleResolver (ModuleId (..))

spec :: Spec
spec = describe "parseAst" $ do
    it "simple happy path" $ do
        let existingFiles = [[osp|/home/repo/src/lib/demo.ts|]]
        let prog =
                TsModule
                    { path = [osp|/home/repo/src/lib/demo.ts|]
                    , cst =
                        [ Import
                            { prefix = "import * from'"
                            , target = "@/types/errors"
                            , suffix = "';"
                            }
                        ]
                    }
        ast <-
            runEff
                . runMockRoFileSystem (mockFiles existingFiles)
                . runReader @TsConfig defaultTsConfig
                $ parseAst prog
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
        ast <-
            runEff
                . runReader @TsConfig emptyTsConfig
                . runFileSystemIO
                $ parseAst prog
        ast
            `shouldBe` AstModule
                { id = ModuleId "src/main"
                , nodes =
                    [ ImportNode {target = ModuleId "react"}
                    , ImportNode {target = ModuleId "src/types/errors"}
                    ]
                }
