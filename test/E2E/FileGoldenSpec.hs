module E2E.FileGoldenSpec (spec) where

import Data.Text qualified as T
import Effects.FileSystem (decodeOsPath)
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (listFixtures, requireRight)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Show.Pretty (ppShow)
import TypeScript.CST
import TypeScript.Lexer (lexer)
import TypeScript.Parser
import TypeScript.Tokens
import Types (Renderable (render))

tsFixturesPath :: OsPath
tsFixturesPath = [osp|test/fixtures/typescript|]

spec :: Spec
spec = do
    describe "TypeScript Tests" $
        runIO (listFixtures tsFixturesPath ".ts") >>= mapM_ tsGoldenTest
  where
    tsGoldenTest :: OsPath -> Spec
    tsGoldenTest filename = do
        let testName = T.unpack . decodeOsPath . takeBaseName $ filename
        let fnameStr = T.unpack . decodeOsPath $ filename

        it ("Lexer " <> testName) $ do
            -- Given
            source <- decodeUtf8 <$> SFO.readFile' (tsFixturesPath </> filename)

            -- When
            let res = runParser lexer fnameStr source

            -- Then
            tokens <- requireRight errorBundlePretty res
            reconstruct tokens `shouldBe` source
            return $ defaultGolden (testName <> "-lexer") (ppShow tokens)

        it ("Parse " <> testName) $ do
            -- Given
            let path = tsFixturesPath </> filename
            source <- decodeUtf8 <$> SFO.readFile' path

            -- When
            let res = parseTs TsFile {path, content = source}

            -- Then
            p <- requireRight id res
            render p.cst `shouldBe` source
            return $ defaultGolden (testName <> "-parser") (ppShow p)

reconstruct :: [TsToken] -> T.Text
reconstruct = foldMap (.raw)
