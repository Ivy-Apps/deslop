module E2E.TranslationsSpec (spec) where

import Deslop
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effects.FileSystem (runFileSystemIO)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils
import UnliftIO.Temporary (withSystemTempDirectory)
import Types
import Effectful.Concurrent (runConcurrent)
import Data.IORef (newIORef)
import Data.IORef.Extra (readIORef)

spec :: Spec
spec = describe "NextJS Translations" $ do
    it "translates ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir
            logsRef <- newIORef Nothing

            -- When
            res <-
                runEff
                    . runFileSystemIO
                    . runErrorNoCallStack @TranslationsError
                    . runCLILogTest logsRef
                    . runAITest
                    . runConcurrent
                    $ translateProject (defaultParams tmpDir)

            -- Then
            res `shouldBe` Right ()
            logs <- readIORef logsRef
            logs `shouldBe` Nothing
            let filesToVerify =
                    [ "messages/es.json"
                    , "messages/fr.json"
                    , "messages/en.json"
                    ]
            fullSnapshot <- snapshot tmpDir filesToVerify
            return $ defaultGolden "translations-1" fullSnapshot
