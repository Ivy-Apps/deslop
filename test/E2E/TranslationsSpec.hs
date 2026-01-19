module E2E.TranslationsSpec where

import Test.Hspec
import TestUtils
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "NextJS Translations" $ do
    it "translates ts-project-1" $ do
        withSystemTempDirectory "deslop-test" $ \tmpDir -> do
            -- Given
            copyDir projectFixturePath tmpDir

            -- When
            -- TBD

            -- Then
            True `shouldBe` True
