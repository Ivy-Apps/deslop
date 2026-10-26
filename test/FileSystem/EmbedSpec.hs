module FileSystem.EmbedSpec (spec) where

import FileSystem.Embed (embedParsed, embedText)
import Test.Hspec

spec :: Spec
spec = describe "FileSystem.Embed" $ do
    it "compiles in a file exactly as it is on disk" $ do
        onDisk <- decodeUtf8 <$> readFileBS "fourmolu.yaml"

        $$(embedText "fourmolu.yaml") `shouldBe` onDisk

    it "compiles in what a file parses to" $ do
        onDisk <- decodeUtf8 <$> readFileBS "fourmolu.yaml"

        $$(embedParsed (Right . lines) "fourmolu.yaml") `shouldBe` lines onDisk
