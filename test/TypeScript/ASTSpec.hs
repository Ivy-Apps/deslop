module TypeScript.ASTSpec (spec) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    EdgeKind (..),
    EdgeTarget (..),
    canonicalName,
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (runFileSystemIO)
import FileSystem.Path (ProjectRoot (..), absPathUnsafe)
import Fixtures.TypeScript.Config (defaultTsConfig, emptyTsConfig)
import System.OsPath (OsPath, osp)
import Test.Hspec
import TypeScript.AST (parseAst)
import TypeScript.Config
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Parser (TsFile (..), parseTs)

repoRoot :: ProjectRoot
repoRoot = ProjectRoot (absPathUnsafe [osp|/home/repo|])

runParseAst :: TsConfig -> [OsPath] -> TsProgram -> IO AstModule
runParseAst cfg existingFiles prog =
    runEff
        . runMockRoFileSystem (mockFiles existingFiles)
        . runReader @TsConfig cfg
        . runReader @ProjectRoot repoRoot
        $ parseAst prog

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
        ast <- runParseAst defaultTsConfig existingFiles prog
        ast
            `shouldBe` AstModule
                { id = moduleIdUnsafe "/home/repo/src/lib/demo.ts"
                , names = moduleNameUnsafe "@/lib/demo" :| []
                , path = absPathUnsafe [osp|/home/repo/src/lib/demo.ts|]
                , nodes =
                    [ DependencyEdge
                        { specifier = moduleNameUnsafe "@/types/errors"
                        , target = Unresolved
                        , kind = ImportEdge
                        , rawStatement = "import * from'@/types/errors';"
                        }
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
        -- The id is deliberately not asserted: it is the canonical path of the
        -- file, which this test reaches through the real filesystem, and
        -- canonicalising is what makes it machine-specific. Nothing renders it.
        (ast.names, ast.path, ast.nodes)
            `shouldBe`
                ( moduleNameUnsafe "/src/main" :| []
                , absPathUnsafe [osp|/home/repo/src/main.ts|]
                ,
                    [ DependencyEdge
                        { specifier = moduleNameUnsafe "react"
                        , target = Unresolved
                        , kind = ImportEdge
                        , rawStatement = "import { useEffect } from 'react';\n"
                        }
                    , DependencyEdge
                        { specifier = moduleNameUnsafe "src/types/errors"
                        , target = Unresolved
                        , kind = ImportEdge
                        , rawStatement = "import type { Error } from 'src/types/errors';"
                        }
                    ]
                )

    -- A file no `paths` entry covers falls back to its own path for a name.
    -- That name is a Rulebook Rule's match target and a Rule Violation's
    -- Problem Id, so an absolute one names this machine in a committed
    -- baseline. The leading '/' is what keeps it out of the namespace bare
    -- package specifiers live in: a root-level `next-auth.d.ts` importing the
    -- `next-auth` package must not come out importing itself.
    it "names a file outside every paths mapping from the project root" $ do
        let prog =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/next.config.ts|]
                    , cst = []
                    }
        ast <- runParseAst defaultTsConfig [] prog
        canonicalName ast `shouldBe` moduleNameUnsafe "/next.config"

    it "a barrel answers to its directory form as well as its index form" $ do
        let existingFiles =
                [ [osp|/home/repo/src/features/home/index.ts|]
                , [osp|/home/repo/src/features/home/service.ts|]
                ]
        let prog =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/src/features/home/index.ts|]
                    , cst = []
                    }
        ast <- runParseAst defaultTsConfig existingFiles prog
        ast.names
            `shouldBe` moduleNameUnsafe "@/features/home/index"
                :| [moduleNameUnsafe "@/features/home"]

    it "a re-export resolves to the same module an import of it would" $ do
        let existingFiles =
                [ [osp|/home/repo/src/features/home/index.ts|]
                , [osp|/home/repo/src/features/home/service.ts|]
                ]
        let barrel =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/src/features/home/index.ts|]
                    , cst =
                        [ ReExport
                            { prefix = "export * from \""
                            , target = "@/features/home/service"
                            , suffix = "\";"
                            }
                        ]
                    }
        ast <- runParseAst defaultTsConfig existingFiles barrel
        ast.nodes
            `shouldBe` [ DependencyEdge
                            { specifier = moduleNameUnsafe "@/features/home/service"
                            , target = ToModule (moduleIdUnsafe "/home/repo/src/features/home/service.ts")
                            , kind = ReExportEdge
                            , rawStatement = "export * from \"@/features/home/service\";"
                            }
                       ]

    it "the directory form of a barrel resolves to the barrel itself" $ do
        let existingFiles =
                [ [osp|/home/repo/src/app/page.ts|]
                , [osp|/home/repo/src/features/home/index.ts|]
                ]
        let page =
                TsModule
                    { path = absPathUnsafe [osp|/home/repo/src/app/page.ts|]
                    , cst =
                        [ Import
                            { prefix = "import { Home } from \""
                            , target = "@/features/home"
                            , suffix = "\";"
                            }
                        ]
                    }
        ast <- runParseAst defaultTsConfig existingFiles page
        map (.target) ast.nodes
            `shouldBe` [ToModule (moduleIdUnsafe "/home/repo/src/features/home/index.ts")]

    it "a re-export of a relative specifier is an edge to the same file" $ do
        let existingFiles =
                [ [osp|/home/repo/src/features/home/index.ts|]
                , [osp|/home/repo/src/features/home/service.ts|]
                ]
        let path = absPathUnsafe [osp|/home/repo/src/features/home/index.ts|]
        prog <-
            either (fail . toString) pure
                . first toText
                . parseTs
                $ TsFile {path = path, content = "export { svc } from './service';\n"}
        ast <- runParseAst defaultTsConfig existingFiles prog
        map (.target) ast.nodes
            `shouldBe` [ToModule (moduleIdUnsafe "/home/repo/src/features/home/service.ts")]
