module Deslop.RulebookSpec (spec) where

import Data.Text qualified as T
import Deslop.Rulebook
import Effectful (runEff)
import Effects.FileSystem (decodeOsPath, fsMkAbsolute, runFileSystemIO)
import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (listFixtures)
import Text.Show.Pretty (ppShow)

rbFixturesPath :: OsPath
rbFixturesPath = [osp|test/fixtures/rulebook|]

spec :: Spec
spec = do
    describe "parseRulebookYaml" $
        runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ parseRuleBookTest
    describe "rulebookFromFile" $
        runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ ruleBookFromFileTest
  where
    parseRuleBookTest :: OsPath -> Spec
    parseRuleBookTest fpath = do
        let testName = T.unpack $ "rulebook-dto-from-yaml--" <> decodeOsPath (takeBaseName fpath)
        it ("case: " <> testName) $ do
            ruleBookYaml <- SFO.readFile' (rbFixturesPath </> fpath)
            let ruleBookRes = parseRuleBookYaml ruleBookYaml
            return $ defaultGolden testName (ppShow ruleBookRes)

    ruleBookFromFileTest :: OsPath -> Spec
    ruleBookFromFileTest fpath = do
        let testName = T.unpack $ "rulebook-from-file--" <> decodeOsPath (takeBaseName fpath)
        it ("case: " <> testName) $ do
            res <- runEff . runFileSystemIO $ do
                fsMkAbsolute (rbFixturesPath </> fpath) >>= ruleBookFromFile
            return $ defaultGolden testName (ppShow res)
