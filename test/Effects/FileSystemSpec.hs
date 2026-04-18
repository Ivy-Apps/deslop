module Effects.FileSystemSpec (spec) where

import Effectful (runEff)
import Effects.FileSystem (AbsPath (..), decodeOsPath, encodeOsPath, encodeOsPathString, fsListDirectory, fsMkAbsolute, runRoFileSystemIO)
import System.Directory.OsPath qualified as SDO
import System.OsPath (isAbsolute, osp)
import System.OsString qualified as OS
import Test.Hspec

spec :: Spec
spec = describe "Effects.FileSystem" $ do
    it "fsMkAbsolute" $ do
        -- Given
        let path = [osp|./test/../deslop.cabal|]

        -- When
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- Then
        OS.length absPath.osPath `shouldSatisfy` (> OS.length path)
        absPath.osPath `shouldSatisfy` isAbsolute
        SDO.doesFileExist absPath.osPath `shouldReturn` True

    it "fsListDirectory" $ do
        -- Given
        let path = [osp|test/fixtures/static|]
        absPath <- runEff . runRoFileSystemIO $ fsMkAbsolute path

        -- When
        entries <- runEff . runRoFileSystemIO $ fsListDirectory absPath

        -- Then
        entries `shouldSatisfy` all (isAbsolute . (.osPath))

    it "encodeOsPath" $ do
        let validText = "src/Main.hs"
        let result = encodeOsPath validText
        result `shouldBe` [osp|src/Main.hs|]

    it "encode <> decode OsPath" $ do
        let fp = "src/Main.hs" :: FilePath
        let osPath = encodeOsPathString fp
        osPath `shouldBe` [osp|src/Main.hs|]
        let ospText = decodeOsPath osPath
        ospText `shouldBe` "src/Main.hs"
        encodeOsPath ospText `shouldBe` [osp|src/Main.hs|]
