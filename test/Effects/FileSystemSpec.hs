module Effects.FileSystemSpec (spec) where

import Effectful (runEff)
import Effects.FileSystem (AbsPath (..), fsListAbsDirectory, fsMkAbsolute, runRoFileSystemIO)
import System.Directory.OsPath qualified as SDO
import System.OsPath (isAbsolute, osp)
import System.OsString qualified as OS
import Test.Hspec

spec :: Spec
spec = describe "FileSystem" $ do
    it "fsMkAbsolute" $ do
        -- Given
        let path = [osp|./test/../deslop.cabal|]

        -- When
        AbsPath absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- Then
        (OS.length absPath) `shouldSatisfy` (> OS.length path)
        absPath `shouldSatisfy` isAbsolute
        SDO.doesFileExist absPath `shouldReturn` True

    it "fsListAbsDirectory" $ do
        -- Given
        let path = [osp|test/fixtures/static|]
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- When
        entries <- runEff . runRoFileSystemIO $ fsListAbsDirectory absPath

        -- Then
        entries `shouldSatisfy` (and . fmap (isAbsolute . (.osPath)))
