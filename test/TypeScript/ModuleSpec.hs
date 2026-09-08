module TypeScript.ModuleSpec (spec) where

import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    canonicalName,
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful
import Effectful.Reader.Static (runReader)
import FileSystem.Path (ProjectRoot (..), absPathUnsafe)
import Fixtures.TypeScript.Config (defaultTsConfig, emptyTsConfig)
import System.OsPath (OsPath, osp)
import Test.Hspec
import TestUtils (rp)
import TypeScript.Config
import TypeScript.CST (TsNode (..), TsProgram (..))
import TypeScript.Module (parseModule)
import TypeScript.Parser (TsFile (..), parseTs)

repoRoot :: ProjectRoot
repoRoot = ProjectRoot (absPathUnsafe [osp|/home/repo|])

runParseAst :: TsConfig -> [OsPath] -> TsProgram -> IO Module
runParseAst cfg existingFiles prog =
    runEff
        . runMockRoFileSystem (mockFiles existingFiles)
        . runReader @TsConfig cfg
        . runReader @ProjectRoot repoRoot
        $ parseModule prog

spec :: Spec
spec = describe "TypeScript.Module" $ do
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
            `shouldBe` Module
                { id = moduleIdUnsafe "/home/repo/src/lib/demo.ts"
                , names = moduleNameUnsafe "@/lib/demo" :| []
                , path = rp "src/lib/demo.ts"
                , edges =
                    [ DependencyEdge
                        { specifier = specifierUnsafe "@/types/errors"
                        , target = External (specifierUnsafe "@/types/errors")
                        , kind = ImportEdge
                        , location =
                            Location
                                { file = rp "src/lib/demo.ts"
                                , line = 1
                                , code = "import * from'@/types/errors';"
                                }
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
        ast <- runParseAst emptyTsConfig [] prog
        -- The id is deliberately not asserted: it is the canonical path of the
        -- file, and canonicalising is what makes it machine-specific. Nothing
        -- renders it.
        (ast.names, ast.path, ast.edges)
            `shouldBe`
                ( moduleNameUnsafe "/src/main" :| []
                , rp "src/main.ts"
                ,
                    [ DependencyEdge
                        { specifier = specifierUnsafe "react"
                        , target = External (specifierUnsafe "react")
                        , kind = ImportEdge
                        , location =
                            Location
                                { file = rp "src/main.ts"
                                , line = 1
                                , code = "import { useEffect } from 'react';\n"
                                }
                        }
                    , DependencyEdge
                        { specifier = specifierUnsafe "src/types/errors"
                        , target = External (specifierUnsafe "src/types/errors")
                        , kind = ImportEdge
                        , location =
                            Location
                                { file = rp "src/main.ts"
                                , line = 2
                                , code = "import type { Error } from 'src/types/errors';"
                                }
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
        ast.edges
            `shouldBe` [ DependencyEdge
                            { specifier = specifierUnsafe "@/features/home/service"
                            , target =
                                Resolved
                                    (moduleIdUnsafe "/home/repo/src/features/home/service.ts")
                                    (moduleNameUnsafe "@/features/home/service" :| [])
                            , kind = ReExportEdge
                            , location =
                                Location
                                    { file = rp "src/features/home/index.ts"
                                    , line = 1
                                    , code = "export * from \"@/features/home/service\";"
                                    }
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
        map (.target) ast.edges
            `shouldBe` [ Resolved
                            (moduleIdUnsafe "/home/repo/src/features/home/index.ts")
                            (moduleNameUnsafe "@/features/home/index" :| [moduleNameUnsafe "@/features/home"])
                       ]

    -- An import's alias and a re-export's run in opposite directions: in
    -- `import { A as B } from './m'`, `A` is what './m' exports; in
    -- `export { A as B } from './m'`, `A` is what './m' exports and `B` is what
    -- a consumer of the barrel imports. Deslop stores neither - an edge names
    -- the module - so the two statements are the same edge, differing only in
    -- the kind that decides how a report words itself.
    it "an aliased import and an aliased re-export are the same edge" $ do
        let existingFiles =
                [ [osp|/home/repo/src/features/home/index.ts|]
                , [osp|/home/repo/src/features/home/service.ts|]
                ]
        let path = absPathUnsafe [osp|/home/repo/src/features/home/index.ts|]
        let parseOf source =
                either (fail . toString) pure
                    . first toText
                    . parseTs
                    $ TsFile {path = path, content = source}

        imported <- parseOf "import { A as B } from '@/features/home/service';\n"
        reExported <- parseOf "export { A as B } from '@/features/home/service';\n"
        importAst <- runParseAst defaultTsConfig existingFiles imported
        reExportAst <- runParseAst defaultTsConfig existingFiles reExported

        map (.target) reExportAst.edges `shouldBe` map (.target) importAst.edges
        map (.specifier) reExportAst.edges `shouldBe` map (.specifier) importAst.edges
        map (.kind) importAst.edges `shouldBe` [ImportEdge]
        map (.kind) reExportAst.edges `shouldBe` [ReExportEdge]

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
        map (.target) ast.edges
            `shouldBe` [ Resolved
                            (moduleIdUnsafe "/home/repo/src/features/home/service.ts")
                            (moduleNameUnsafe "@/features/home/service" :| [])
                       ]
