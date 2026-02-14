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
                ]
        forM_ cases $ \(input, expected) ->
            it input $ do
                let file = TsFile "test.ts" (T.pack input)
                case parseTs file of
                    Left err -> expectationFailure err
                    Right program ->
                        program.ast `shouldBe` [expected]
