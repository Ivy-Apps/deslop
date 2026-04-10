module TypeScript.ConfigSpec (spec) where

import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import TestUtils (fixturesBasePath)
import TypeScript.Config (ImportAlias (..), TsConfig (..), parseTsConfig)

tsConfigFixturesPath :: OsPath
tsConfigFixturesPath = fixturesBasePath </> [osp|typescript|]

spec :: Spec
spec = describe "parseTsConfig" $ do
    it "returns Nothing for invalid JSON" $ do
        content <- SFO.readFile' (tsConfigFixturesPath </> [osp|tsconfig-invalid.json|])
        parseTsConfig content `shouldBe` Nothing

    it "returns Nothing when compilerOptions.paths is missing" $ do
        let content = encodeUtf8 @Text "{\"compilerOptions\": {}}"
        parseTsConfig content `shouldBe` Nothing

    it "parses simple tsconfig with one path alias" $ do
        content <- SFO.readFile' (tsConfigFixturesPath </> [osp|tsconfig-simple.json|])
        parseTsConfig content
            `shouldBe` Just
                ( TsConfig
                    { paths =
                        [ ImportAlias {label = "@/", path = "src/"}
                        ]
                    }
                )

    it "parses complex tsconfig with multiple path aliases sorted by longest label first" $ do
        content <- SFO.readFile' (tsConfigFixturesPath </> [osp|tsconfig-complex.json|])
        parseTsConfig content
            `shouldBe` Just
                ( TsConfig
                    { paths =
                        [ ImportAlias {label = "@components/", path = "src/components/"}
                        , ImportAlias {label = "@assets/", path = "src/assets/"}
                        , ImportAlias {label = "@hooks/", path = "src/hooks/"}
                        , ImportAlias {label = "@utils/", path = "src/utils/"}
                        , ImportAlias {label = "@/", path = "src/"}
                        ]
                    }
                )
