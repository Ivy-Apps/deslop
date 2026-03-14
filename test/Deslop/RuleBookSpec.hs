module Deslop.RuleBookSpec (spec) where

import Test.Hspec
import TestUtils (listFixtures)
import System.FilePath (takeBaseName, (</>))
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Deslop.RuleBook (parseRuleBookYaml)
import Test.Hspec.Golden (defaultGolden)
import Text.Show.Pretty (ppShow)

rbFixturesPath :: FilePath
rbFixturesPath = "test/fixtures/rulebook"

spec :: Spec
spec = do
    describe "parseRuleBookYaml" $ 
      runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ parseRuleBookTest
    where
      parseRuleBookTest:: FilePath -> Spec
      parseRuleBookTest fpath = do
        let testName = takeBaseName fpath
        it ("case:" <> testName) $ do
            ruleBookYaml <- TE.encodeUtf8 <$> TIO.readFile (rbFixturesPath </> fpath)
            let ruleBookRes = parseRuleBookYaml ruleBookYaml
            return $ defaultGolden testName (ppShow ruleBookRes)
