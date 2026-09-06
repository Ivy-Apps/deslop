module TypeScript.Config.DtoSpec (spec) where

import Data.Map qualified as M
import Test.Hspec
import TypeScript.Config.Dto (
    CompilerOptionsDto (..),
    ExtendsDto (..),
    TsConfigDto (..),
    parseTsConfigJson,
    stripTsComments,
 )

spec :: Spec
spec = describe "TypeScript.Config.Dto" $ do
    describe "extends" $ do
        it "reads a single base as a string" $ do
            extendsOf "{\"extends\": \"./tsconfig.base.json\"}"
                `shouldBe` Right (Just (ExtendsOne "./tsconfig.base.json"))

        it "reads an array of bases, in the order they were written" $ do
            extendsOf "{\"extends\": [\"./a.json\", \"./b.json\"]}"
                `shouldBe` Right (Just (ExtendsMany ["./a.json", "./b.json"]))

        it "reads an empty array as extending nothing" $ do
            extendsOf "{\"extends\": []}" `shouldBe` Right (Just (ExtendsMany []))

        it "is absent when the config does not extend anything" $ do
            extendsOf "{\"compilerOptions\": {}}" `shouldBe` Right Nothing

        it "is malformed, not a decoding failure, when it is not a string" $ do
            extendsOf "{\"extends\": 42}" `shouldBe` Right (Just ExtendsMalformed)

        it "is malformed when an array holds anything but strings" $ do
            extendsOf "{\"extends\": [\"./a.json\", 42]}"
                `shouldBe` Right (Just ExtendsMalformed)

    describe "compilerOptions" $ do
        it "is absent in a config that only extends" $ do
            optionsOf "{\"extends\": \"./a.json\"}" `shouldBe` Right Nothing

        it "reads baseUrl and paths" $ do
            optionsOf "{\"compilerOptions\": {\"baseUrl\": \".\", \"paths\": {\"@/*\": [\"./src/*\"]}}}"
                `shouldBe` Right
                    ( Just
                        CompilerOptionsDto
                            { baseUrl = Just "."
                            , paths = Just (M.fromList [("@/*", ["./src/*"])])
                            }
                    )

        it "tells an absent paths apart from an empty one, which still overrides" $ do
            fmap (>>= (.paths)) (optionsOf "{\"compilerOptions\": {\"paths\": {}}}")
                `shouldBe` Right (Just mempty)

    describe "JSONC" $ do
        it "decodes a config carrying line and block comments" $ do
            extendsOf jsoncConfig `shouldBe` Right (Just (ExtendsOne "./base.json"))

        it "leaves a URL inside a string literal alone" $ do
            stripTsComments "{\"a\": \"http://x.dev\"}" `shouldBe` "{\"a\": \"http://x.dev\"}"

        it "fails on JSON it cannot decode" $ do
            parseTsConfigJson "{\"invalid\"}" `shouldSatisfy` isLeft

jsoncConfig :: ByteString
jsoncConfig =
    "{\n"
        <> "  // the shared base\n"
        <> "  \"extends\": \"./base.json\" /* not \"./other.json\" */\n"
        <> "}"

extendsOf :: ByteString -> Either Text (Maybe ExtendsDto)
extendsOf = fmap (.extends) . parseTsConfigJson

optionsOf :: ByteString -> Either Text (Maybe CompilerOptionsDto)
optionsOf = fmap (.compilerOptions) . parseTsConfigJson
