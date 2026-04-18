module Deslop.ASTSpec (spec) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    parseAst,
 )
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (absPathUnsafe, runFileSystemIO)
import System.OsPath (osp)
import Test.Hspec
import TestUtils (defaultTsConfig, emptyTsConfig)
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Config
import TypeScript.ModuleResolver (moduleIdUnsafe)

spec :: Spec
spec = describe "Deslop.AST" $ do
    it "simple happy path" $ do
        let existingFiles = [[osp|/home/repo/src/lib/demo.ts|]]
        let prog =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/src/lib/demo.ts|]
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
                { id = moduleIdUnsafe "@/lib/demo"
                , nodes =
                    [ ImportNode {target = moduleIdUnsafe "@/types/errors"}
                    ]
                }

    it "import alias not available" $ do
        let prog =
                TsModule
                    { path = absPathUnsafe [osp|src/main.ts|]
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
                { id = moduleIdUnsafe "src/main"
                , nodes =
                    [ ImportNode {target = moduleIdUnsafe "react"}
                    , ImportNode {target = moduleIdUnsafe "src/types/errors"}
                    ]
                }
