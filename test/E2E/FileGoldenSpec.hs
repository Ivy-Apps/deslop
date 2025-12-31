module E2E.FileGoldenSpec (spec) where

import Data.IORef (newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Deslop (deslopFile)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (runFileSystemTest)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Show.Pretty (ppShow)
import TypeScript.AST
import TypeScript.Config (TsConfig (TsConfig), parseTsConfig)
import TypeScript.Lexer (lexer)
import TypeScript.Parser
import TypeScript.Tokens

tsFixturesPath :: FilePath
tsFixturesPath = "test/fixtures/typescript"

spec :: Spec
spec = do
    describe "TypeScript Tests" $
        (runIO $ listFixtures tsFixturesPath ".ts") >>= mapM_ tsGoldenTest

    describe "TSConfig Tests" $
        (runIO $ listFixtures tsFixturesPath ".json") >>= mapM_ configGoldenTest
  where
    configGoldenTest :: FilePath -> Spec
    configGoldenTest fname = do
        let testName = takeBaseName fname

        it ("case: " <> testName) $ do
            -- Given
            cfgFile <- T.encodeUtf8 <$> TIO.readFile (tsFixturesPath </> fname)

            -- When
            let cfg = parseTsConfig cfgFile

            -- Then
            return $ defaultGolden (testName) (ppShow cfg)

    tsGoldenTest :: FilePath -> Spec
    tsGoldenTest filename = do
        let testName = takeBaseName filename

        it ("Lexer " <> testName) $ do
            -- Given
            source <- TIO.readFile (tsFixturesPath </> filename)

            -- When
            let res = runParser lexer filename source

            -- Then
            case res of
                Left e -> fail $ errorBundlePretty e
                Right tokens -> do
                    reconstruct tokens `shouldBe` source
                    return $ defaultGolden (testName <> "-lexer") (ppShow tokens)

        it ("Parse " <> testName) $ do
            -- Given
            let path = tsFixturesPath </> filename
            source <- TIO.readFile path

            -- When
            let res = parseTs TsFile {path = path, content = source}

            -- Then
            case res of
                Left e -> fail e
                Right p -> do
                    renderAst p.ast `shouldBe` source
                    return $ defaultGolden (testName <> "-parser") (ppShow p)

        it ("Deslop " <> testName) $ do
            -- Given
            let path = tsFixturesPath </> filename
            captureRef <- newIORef Nothing
            let tsCfg = TsConfig [] -- TODO: Load a real one

            -- When
            runEff
                . runFileSystemTest captureRef
                . runReader tsCfg
                $ deslopFile path "_ignored.ts"

            -- Then
            actualRes <- readIORef captureRef
            case actualRes of
                Nothing -> fail "The program did not write any output!"
                Just actual -> do
                    let actualContent = T.unpack $ decodeUtf8 actual
                    return $ defaultGolden (testName <> "-deslop") actualContent

listFixtures :: FilePath -> String -> IO [FilePath]
listFixtures dir ext = do
    files <- listDirectory dir
    return $ filter (\f -> takeExtension f == ext) files

reconstruct :: [TsToken] -> T.Text
reconstruct = foldMap (.raw)