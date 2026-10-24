{- | Real TypeScript files, each read off disk and taken through the lexer and
the parser: both must give back every byte, and the parser must find exactly
the dependencies the file declares - no fewer, and none from a comment.
-}
module E2E.FileSpec (spec) where

import FileSystem.Path (absPathUnsafe, decodeOsPath, encodeOsPathString)
import Fixtures.OnDisk (typescriptFiles)
import Frontend.TypeScript.CST (TsNode (..), TsProgram (..))
import Frontend.TypeScript.Lexer (lexer)
import Frontend.TypeScript.Parser (TsFile (..), parseTs)
import Frontend.TypeScript.Tokens (TsToken (..))
import Renderable (Renderable (render))
import System.File.OsPath qualified as SFO
import System.OsPath ((</>))
import Test.Hspec
import TestUtils (listFixtures, requireRight)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)

-- | A dependency as written: an import, or a re-export, of a specifier.
data Dependency
    = Imports Text
    | ReExports Text
    deriving stock (Show, Eq)

-- | Every fixture file, and the dependencies it declares, in source order.
cases :: [(String, [Dependency])]
cases =
    [ ("01-block-comment.ts", [Imports "./utils"])
    , ("01-imports.ts", [Imports "../../lib/utils", Imports "../../lib/logger", Imports "../models/User"])
    , ("01-line-comments.ts", [Imports "../../tests/viewmodel-test"])
    , ("02-block-comment.ts", [Imports "./utils"])
    , ("02-multiline-import.ts", [Imports "../../lib/shop/constants", Imports "react", Imports "./abc"])
    , -- What is commented out is no dependency; a dynamic import is one.
      ("commented-imports.ts", [Imports "@react/hooks", Imports "../new/data"])
    ]

spec :: Spec
spec = describe "E2E.File" $ do
    -- Otherwise a file added to the fixtures would be silently left unread.
    it "reads every fixture file" $ do
        files <- listFixtures typescriptFiles ".ts"

        sort (fmap (toString . decodeOsPath) files) `shouldBe` sort (fmap fst cases)

    for_ cases readsFile

readsFile :: (String, [Dependency]) -> Spec
readsFile (file, dependencies) = do
    let path = typescriptFiles </> encodeOsPathString file
        readSource = decodeUtf8 <$> SFO.readFile' path

    it ("lexes " <> file <> " back to its bytes") $ do
        source <- readSource

        tokens <- requireRight errorBundlePretty $ runParser lexer file source

        foldMap (.raw) tokens `shouldBe` source

    it ("parses " <> file <> " back to its bytes, finding every dependency it declares") $ do
        source <- readSource

        let program = parseTs TsFile {path = absPathUnsafe path, content = source}

        render program.cst `shouldBe` source
        mapMaybe dependencyOf program.cst `shouldBe` dependencies
  where
    dependencyOf = \case
        Import {target} -> Just (Imports target)
        ReExport {target} -> Just (ReExports target)
        Source {} -> Nothing
