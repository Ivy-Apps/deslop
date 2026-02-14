module TypeScript.ParserSpec where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import TypeScript.AST
import TypeScript.Parser

spec :: Spec
spec = do
    describe "Imports" $ do
        let cases =
                [
                    ( "import * from '@/lib/utils'"
                    , Import "import * from '" "@/lib/utils" "'"
                    )
                ,
                    ( "import { \"hello\" as hell } from \"./Context\"\n"
                    , Import "import { \"hello\" as hell } from \"" "./Context" "\"\n"
                    )
                ,
                    ( "import '../../tests/viewmodel-test';"
                    , Import "import '" "../../tests/viewmodel-test" "';"
                    )
                ,
                    ( "await import ('../heavy-module');"
                    , Import "import ('" "../heavy-module" "');"
                    )
                ,
                    ( "await import ('../../lib/extra').extras;"
                    , Import "import ('" "../../lib/extra" "')"
                    )
                ]
        forM_ cases $ \(input, expected) ->
            it input $ do
                let file = TsFile "test.ts" (T.pack input)
                case parseTs file of
                    Left err -> expectationFailure err
                    Right program -> do
                        let importsOnly = filter isImport program.ast
                        importsOnly `shouldBe` [expected]

isImport :: TsNode -> Bool
isImport Import {} = True
isImport _ = False
