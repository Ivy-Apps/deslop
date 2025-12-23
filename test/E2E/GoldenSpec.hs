module E2E.GoldenSpec (spec) where

import Data.IORef (newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import Deslop (deslopFile)
import Effectful (runEff)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (runFileSystemTest)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Show.Pretty (ppShow)
import TypeScript.Lexer (lexer)
import TypeScript.Tokens

tsFixturesPath :: FilePath
tsFixturesPath = "test/fixtures/typescript"

spec :: Spec
spec = describe "E2E Golden Tests" $ do
    inputFiles <- runIO $ listFixtures tsFixturesPath

    mapM_ createGoldenTest inputFiles
  where
    createGoldenTest :: FilePath -> Spec
    createGoldenTest filename = do
        let testName = takeBaseName filename

        it ("Lexer " ++ testName) $ do
            -- Given
            sourceCode <- TIO.readFile (tsFixturesPath </> filename)

            -- When
            let res = runParser lexer filename sourceCode

            -- Then
            case res of
                Left e -> fail . errorBundlePretty $ e
                Right tokens -> do
                    reconstruct tokens `shouldBe` sourceCode
                    return $ defaultGolden (testName <> "-lexer") (ppShow tokens)

        it ("Deslop " ++ testName) $ do
            -- Given
            let inputPath = tsFixturesPath </> filename
            captureRef <- newIORef Nothing

            -- When
            runEff $ runFileSystemTest captureRef $ do
                deslopFile inputPath "_ignored.ts"

            -- Then
            actualResult <- readIORef captureRef
            case actualResult of
                Nothing -> fail "The program did not write any output!"
                Just actualBytes -> do
                    let actualContent = T.unpack $ decodeUtf8 actualBytes
                    return $ defaultGolden (testName <> "-deslop") actualContent

listFixtures :: FilePath -> IO [FilePath]
listFixtures dir = do
    files <- listDirectory dir
    return $ filter (\f -> takeExtension f == ".ts") files

reconstruct :: [TsToken] -> T.Text
reconstruct = foldMap (.raw)