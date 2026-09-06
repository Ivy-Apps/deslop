module TypeScript.ASTSpec (spec) where

import Deslop.AST (AstModule (..), AstNode (..), moduleIdUnsafe)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (runFileSystemIO)
import FileSystem.Path (ProjectRoot (..), absPathUnsafe)
import Fixtures.TypeScript.Config (defaultTsConfig, emptyTsConfig)
import System.OsPath (osp)
import Test.Hspec
import TypeScript.AST (parseAst)
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Config

repoRoot :: ProjectRoot
repoRoot = ProjectRoot (absPathUnsafe [osp|/home/repo|])

spec :: Spec
spec = describe "TypeScript.AST" $ do
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
                . runReader @ProjectRoot repoRoot
                $ parseAst prog
        ast
            `shouldBe` AstModule
                { id = moduleIdUnsafe "@/lib/demo"
                , path = absPathUnsafe [osp|/home/repo/src/lib/demo.ts|]
                , nodes =
                    [ ImportNode {target = moduleIdUnsafe "@/types/errors", rawStatement = "import * from'@/types/errors';"}
                    ]
                }

    it "import alias not available" $ do
        let prog =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/src/main.ts|]
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
                . runReader @ProjectRoot repoRoot
                . runFileSystemIO
                $ parseAst prog
        ast
            `shouldBe` AstModule
                { id = moduleIdUnsafe "/src/main"
                , path = absPathUnsafe [osp|/home/repo/src/main.ts|]
                , nodes =
                    [ ImportNode {target = moduleIdUnsafe "react", rawStatement = "import { useEffect } from 'react';\n"}
                    , ImportNode {target = moduleIdUnsafe "src/types/errors", rawStatement = "import type { Error } from 'src/types/errors';"}
                    ]
                }

    -- A file no `paths` entry covers falls back to its own path for an id.
    -- That id is a Rulebook Rule's match target and a Rule Violation's Problem
    -- Id, so an absolute one names this machine in a committed baseline. The
    -- leading '/' is what keeps it out of the namespace bare package
    -- specifiers live in: a root-level `next-auth.d.ts` importing the
    -- `next-auth` package must not come out importing itself.
    it "names a file outside every paths mapping from the project root" $ do
        let prog =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/next.config.ts|]
                    , cst = []
                    }
        ast <-
            runEff
                . runMockRoFileSystem (mockFiles [])
                . runReader @TsConfig defaultTsConfig
                . runReader @ProjectRoot repoRoot
                $ parseAst prog
        ast.id `shouldBe` moduleIdUnsafe "/next.config"
