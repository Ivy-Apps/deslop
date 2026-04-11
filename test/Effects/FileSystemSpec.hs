module Effects.FileSystemSpec (spec) where

import Effectful (runEff)
import Effects.FileSystem (AbsPath (..), encodeOsPath, fsListAbsDirectory, fsMkAbsolute, runRoFileSystemIO)
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
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- Then
        OS.length absPath.osPath `shouldSatisfy` (> OS.length path)
        absPath.osPath `shouldSatisfy` isAbsolute
        SDO.doesFileExist absPath.osPath `shouldReturn` True

    it "fsListAbsDirectory" $ do
        -- Given
        let path = [osp|test/fixtures/static|]
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- When
        entries <- runEff . runRoFileSystemIO $ fsListAbsDirectory absPath

        -- Then
        entries `shouldSatisfy` all (isAbsolute . (.osPath))

    it "encodeOsPath" $ do
        -- Given
        let validText = "src/Main.hs"

        -- When
        let result = encodeOsPath validText

        -- Then
        result `shouldBe` [osp|src/Main.hs|]
