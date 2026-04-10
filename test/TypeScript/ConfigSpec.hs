module TypeScript.ConfigSpec (spec) where

import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import TestUtils (fixturesBasePath)
import TypeScript.Config (ImportAlias (..), TsConfigLegacy (..), parseTsConfigLegacy)

tsConfigFixturesPath :: OsPath
tsConfigFixturesPath = fixturesBasePath </> [osp|typescript|]

spec :: Spec
spec = describe "parseTsConfig" $ do
    it "returns Nothing for invalid JSON" $ do
        content <- SFO.readFile' (tsConfigFixturesPath </> [osp|tsconfig-invalid.json|])
        parseTsConfigLegacy content `shouldBe` Nothing

    it "returns Nothing when compilerOptions.paths is missing" $ do
        let content = encodeUtf8 @Text "{\"compilerOptions\": {}}"
        parseTsConfigLegacy content `shouldBe` Nothing

    it "parses simple tsconfig with one path alias" $ do
        content <- SFO.readFile' (tsConfigFixturesPath </> [osp|tsconfig-simple.json|])
        parseTsConfigLegacy content
            `shouldBe` Just
                ( TsConfigLegacy
                    { paths =
                        [ ImportAlias {label = "@/", path = "src/"}
                        ]
                    }
                )

    it "parses complex tsconfig with multiple path aliases sorted by longest label first" $ do
        content <- SFO.readFile' (tsConfigFixturesPath </> [osp|tsconfig-complex.json|])
        parseTsConfigLegacy content
            `shouldBe` Just
                ( TsConfigLegacy
                    { paths =
                        [ ImportAlias {label = "@components/", path = "src/components/"}
                        , ImportAlias {label = "@assets/", path = "src/assets/"}
                        , ImportAlias {label = "@hooks/", path = "src/hooks/"}
                        , ImportAlias {label = "@utils/", path = "src/utils/"}
                        , ImportAlias {label = "@/", path = "src/"}
                        ]
                    }
                )

    it "strips line comments outside of strings" $ do
        let content = encodeUtf8 @Text "{ \"compilerOptions\": { \"paths\": { \"@/\": [\"src/\"] } } } // this is a line comment"
        parseTsConfigLegacy content `shouldBe` Just (TsConfigLegacy {paths = [ImportAlias {label = "@/", path = "src/"}]})

    it "strips block comments outside of strings" $ do
        let content = encodeUtf8 @Text "{ \"compilerOptions\": /* block comment */ { \"paths\": { \"@/\": [\"src/\"] } } }"
        parseTsConfigLegacy content `shouldBe` Just (TsConfigLegacy {paths = [ImportAlias {label = "@/", path = "src/"}]})

    it "does not strip line comment markers inside strings" $ do
        let content = encodeUtf8 @Text "{ \"compilerOptions\": { \"paths\": { \"@//\": [\"src//dir/\"] } } }"
        parseTsConfigLegacy content `shouldBe` Just (TsConfigLegacy {paths = [ImportAlias {label = "@//", path = "src//dir/"}]})
