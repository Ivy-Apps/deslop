module Deslop.RulebookSpec (spec) where

import Data.Text qualified as T
import Deslop.Rulebook
import Effectful (runEff)
import Effects.FileSystem (decodeOsPath, runFileSystemIO)
import System.OsPath (OsPath, osp, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (listFixtures, mkAbsolute)
import Text.Show.Pretty (ppShow)

rbFixturesPath :: OsPath
rbFixturesPath = [osp|test/fixtures/rulebook|]

spec :: Spec
spec = describe "Deslop.Rulebook" $ do
    describe "rulebookFromFile" $
        runIO (listFixtures rbFixturesPath ".yaml") >>= mapM_ ruleBookFromFileTest
  where
    ruleBookFromFileTest :: OsPath -> Spec
    ruleBookFromFileTest fpath = do
        let testName = T.unpack $ "rulebook-from-file--" <> decodeOsPath (takeBaseName fpath)
        it ("case: " <> testName) $ do
            rbPath <- mkAbsolute (rbFixturesPath </> fpath)
            res <- runEff . runFileSystemIO $ ruleBookFromFile rbPath
            return $ defaultGolden testName (ppShow res)
