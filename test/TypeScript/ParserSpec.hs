module TypeScript.ParserSpec (spec) where

import Data.Text qualified as T
import FileSystem.Path (absPathUnsafe)
import System.OsPath (osp)
import Test.Hspec
import TypeScript.CST
import TypeScript.Parser

spec :: Spec
spec = do
    describe "TypeScript.Parser" $ do
        let cases =
                [
                    ( "import * from '@/lib/utils'"
                    , Import
                        { prefix = "import * from '"
                        , target = "@/lib/utils"
                        , suffix = "'"
                        }
                    )
                ,
                    ( "import { \"hello\" as hell } from \"./Context\"\n"
                    , Import
                        { prefix = "import { \"hello\" as hell } from \""
                        , target = "./Context"
                        , suffix = "\"\n"
                        }
                    )
                ,
                    ( "import '../../tests/viewmodel-test';"
                    , Import
                        { prefix = "import '"
                        , target = "../../tests/viewmodel-test"
                        , suffix = "';"
                        }
                    )
                ,
                    ( "await import ('../heavy-module');"
                    , Import
                        { prefix = "import ('"
                        , target = "../heavy-module"
                        , suffix = "');"
                        }
                    )
                ,
                    ( "await import ('../../lib/extra').extras;"
                    , Import
                        { prefix = "import ('"
                        , target = "../../lib/extra"
                        , suffix = "')"
                        }
                    )
                ]
        forM_ cases $ \(input, expected) ->
            it input $ do
                let file = TsFile (absPathUnsafe [osp|test.ts|]) (T.pack input)
                case parseTs file of
                    Left err -> expectationFailure err
                    Right program -> do
                        let importsOnly = filter isImport program.cst
                        importsOnly `shouldBe` [expected]

    -- What the parser calls the target is what `deslop fix` rewrites, so these
    -- pin the specifier against the two ways a re-export can hide something
    -- that looks like one: an aliased name, and a string literal in the export
    -- clause.
    describe "TypeScript.Parser re-export targets" $ do
        let cases =
                [
                    ( "export * from '@/a';"
                    , ReExport {prefix = "export * from '", target = "@/a", suffix = "';"}
                    )
                ,
                    ( "export * as ns from '@/a';"
                    , ReExport {prefix = "export * as ns from '", target = "@/a", suffix = "';"}
                    )
                ,
                    ( "export { x } from \"@/a\";"
                    , ReExport {prefix = "export { x } from \"", target = "@/a", suffix = "\";"}
                    )
                ,
                    -- The alias runs the other way round from an import's: in
                    -- `export { A as B } from './m'`, `A` is what './m' exports
                    -- and `B` is what a consumer of the barrel imports. Neither
                    -- is the module, and Deslop stores neither - an edge names
                    -- './m' and nothing else, which is why the direction cannot
                    -- change what a rule sees.
                    ( "export { A as B } from './m';"
                    , ReExport {prefix = "export { A as B } from '", target = "./m", suffix = "';"}
                    )
                ,
                    ( "export { a, b as c } from './m';"
                    , ReExport {prefix = "export { a, b as c } from '", target = "./m", suffix = "';"}
                    )
                ,
                    -- The clause holds a quoted name, so the specifier is not
                    -- the first string in the statement.
                    ( "export { \"odd-name\" as ok } from '@/a';"
                    , ReExport {prefix = "export { \"odd-name\" as ok } from '", target = "@/a", suffix = "';"}
                    )
                ,
                    ( "export type { T } from '@/a';"
                    , ReExport {prefix = "export type { T } from '", target = "@/a", suffix = "';"}
                    )
                ,
                    ( "export { default as A } from '@/a';"
                    , ReExport {prefix = "export { default as A } from '", target = "@/a", suffix = "';"}
                    )
                ,
                    ( "export {} from '@/a';"
                    , ReExport {prefix = "export {} from '", target = "@/a", suffix = "';"}
                    )
                ]

        forM_ cases $ \(input, expected) ->
            it input $ do
                let file = TsFile (absPathUnsafe [osp|test.ts|]) (T.pack input)
                case parseTs file of
                    Left err -> expectationFailure err
                    Right program ->
                        filter isReExport program.cst `shouldBe` [expected]

isImport :: TsNode -> Bool
isImport Import {} = True
isImport _ = False

isReExport :: TsNode -> Bool
isReExport ReExport {} = True
isReExport _ = False
