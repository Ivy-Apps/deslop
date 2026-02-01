module E2E.TranslationsSpec where

import Deslop
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "NextJS Translations" $ do
    it "translates ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @TranslationsError
                    . runCLILogTest
                    . runAITest
                    $ translateProject (defaultParams tmpDir)

            -- Then
            res `shouldBe` (Right ())
            let filesToVerify =
                    [ "messages/es.json"
                    , "messages/fr.json"
                    , "messages/en.json"
                    ]
            fullSnapshot <- snapshot tmpDir filesToVerify
            return $ defaultGolden "translations-1" fullSnapshot
