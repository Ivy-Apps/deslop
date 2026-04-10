module E2E.FileGoldenSpec (spec) where

import Data.Text qualified as T
import Deslop (deslopFile)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (runReportProblem)
import FsEncoding (decodePathString)
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, takeBaseName, (</>))
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

tsFixturesPath :: OsPath
tsFixturesPath = [osp|test/fixtures/typescript|]

spec :: Spec
spec = do
    describe "TypeScript Tests" $
        runIO (listFixtures tsFixturesPath ".ts") >>= mapM_ tsGoldenTest

    describe "TSConfig Tests" $
        runIO (listFixtures tsFixturesPath ".json") >>= mapM_ configGoldenTest
  where
    configGoldenTest :: OsPath -> Spec
    configGoldenTest fname = do
        let testName = decodePathString (takeBaseName fname)

        it ("case: " <> testName) $ do
            -- Given
            cfgFile <- SFO.readFile' (tsFixturesPath </> fname)

            -- When
            let cfg = parseTsConfig cfgFile

            -- Then
            return $ defaultGolden testName (ppShow cfg)

    tsGoldenTest :: OsPath -> Spec
    tsGoldenTest filename = do
        let testName = decodePathString (takeBaseName filename)
        let fnameStr = decodePathString filename

        it ("Lexer " <> testName) $ do
            -- Given
            source <- decodeUtf8 <$> SFO.readFile' (tsFixturesPath </> filename)

            -- When
            let res = runParser lexer fnameStr source

            -- Then
            case res of
                Left e -> fail $ errorBundlePretty e
                Right tokens -> do
                    reconstruct tokens `shouldBe` source
                    return $ defaultGolden (testName <> "-lexer") (ppShow tokens)

        it ("Parse " <> testName) $ do
            -- Given
            let path = tsFixturesPath </> filename
            source <- decodeUtf8 <$> SFO.readFile' path

            -- When
            let res = parseTs TsFile {path, content = source}

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
                    . runReader (defaultParams [osp|.|])
                    . runCLILogTest logsRef
                    . runReportProblem
                    $ deslopFile path

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
