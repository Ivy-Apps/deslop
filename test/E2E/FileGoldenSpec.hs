module E2E.FileGoldenSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Deslop (deslopFile)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (runReportProblem)
import FsEncoding (encodePathString)
import System.FilePath (takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (defaultParams, listFixtures, runCLILogTest, runFileSystemTest)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Show.Pretty (ppShow)
import TypeScript.CST
import TypeScript.Config (ImportAlias (ImportAlias), TsConfig (..), parseTsConfig)
import TypeScript.Lexer (lexer)
import TypeScript.Parser
import TypeScript.Tokens
import Types (Renderable (render))

tsFixturesPath :: FilePath
tsFixturesPath = "test/fixtures/typescript"

spec :: Spec
spec = do
    describe "TypeScript Tests" $
        runIO (listFixtures tsFixturesPath ".ts") >>= mapM_ tsGoldenTest

    describe "TSConfig Tests" $
        runIO (listFixtures tsFixturesPath ".json") >>= mapM_ configGoldenTest
  where
    configGoldenTest :: FilePath -> Spec
    configGoldenTest fname = do
        let testName = takeBaseName fname

        it ("case: " <> testName) $ do
            -- Given
            cfgFile <- TE.encodeUtf8 <$> TIO.readFile (tsFixturesPath </> fname)

            -- When
            let cfg = parseTsConfig cfgFile

            -- Then
            return $ defaultGolden testName (ppShow cfg)

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
            let res = parseTs TsFile {path = encodePathString path, content = source}

            -- Then
            case res of
                Left e -> fail e
                Right p -> do
                    render p.cst `shouldBe` source
                    return $ defaultGolden (testName <> "-parser") (ppShow p)

        it ("Deslop " <> testName) $ do
            -- Given
            let path = tsFixturesPath </> filename
            fileWriteRef <- newIORef Nothing
            logsRef <- newIORef Nothing
            let tsCfg =
                    TsConfig
                        { paths =
                            [ ImportAlias "@/" "test/"
                            , ImportAlias "@test/" "tests/"
                            ]
                        }

            -- When
            _ <-
                runEff
                    . runFileSystemTest fileWriteRef
                    . runReader tsCfg
                    . runReader (defaultParams ".")
                    . runCLILogTest logsRef
                    . runReportProblem
                    $ deslopFile (encodePathString path)

            -- Then
            actualRes <- readIORef fileWriteRef
            logs <- readIORef logsRef
            logs `shouldBe` Nothing
            case actualRes of
                Nothing -> fail "The program did not write any output!"
                Just actual -> do
                    let actualContent = T.unpack $ decodeUtf8 actual
                    return $ defaultGolden (testName <> "-deslop") actualContent

reconstruct :: [TsToken] -> T.Text
reconstruct = foldMap (.raw)
