module E2E.FileGoldenSpec (spec) where

import Data.Text qualified as T
import FileSystem.Path (absPathUnsafe, decodeOsPath)
import Frontend.TypeScript.CST
import Frontend.TypeScript.Lexer (lexer)
import Frontend.TypeScript.Parser
import Frontend.TypeScript.Tokens
import Renderable (Renderable (render))
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Fixtures.OnDisk (typescriptFiles)
import TestUtils (listFixtures, requireRight)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
    describe "E2E.File" $
        runIO (listFixtures typescriptFiles ".ts") >>= mapM_ tsGoldenTest
  where
    tsGoldenTest :: OsPath -> Spec
    tsGoldenTest filename = do
        let testName = T.unpack . decodeOsPath . takeBaseName $ filename
        let fnameStr = T.unpack . decodeOsPath $ filename

        it ("Lexer " <> testName) $ do
            -- Given
            source <- decodeUtf8 <$> SFO.readFile' (typescriptFiles </> filename)

            -- When
            let res = runParser lexer fnameStr source

            -- Then
            tokens <- requireRight errorBundlePretty res
            reconstruct tokens `shouldBe` source
            return $ defaultGolden ("typescript/" <> testName <> "-lexer") (ppShow tokens)

        it ("Parse " <> testName) $ do
            -- Given
            let path = typescriptFiles </> filename
            source <- decodeUtf8 <$> SFO.readFile' path

            -- When
            let p = parseTs TsFile {path = absPathUnsafe path, content = source}

            -- Then
            render p.cst `shouldBe` source
            return $ defaultGolden ("typescript/" <> testName <> "-parser") (ppShow p)

reconstruct :: [TsToken] -> T.Text
reconstruct = foldMap (.raw)
