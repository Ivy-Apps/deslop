module TypeScript.ParserSpec (spec) where

import Data.Text qualified as T
import FsEncoding (encodePathString)
import Test.Hspec
import TypeScript.CST
import TypeScript.Parser

spec :: Spec
spec = do
    describe "Imports" $ do
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
                let file = TsFile (encodePathString "test.ts") (T.pack input)
                case parseTs file of
                    Left err -> expectationFailure err
                    Right program -> do
                        let importsOnly = filter isImport program.cst
                        importsOnly `shouldBe` [expected]

isImport :: TsNode -> Bool
isImport Import {} = True
isImport _ = False
