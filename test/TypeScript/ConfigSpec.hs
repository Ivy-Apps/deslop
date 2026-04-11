module TypeScript.ConfigSpec (spec) where

import System.File.OsPath qualified as SFO
import System.OsPath (OsPath, osp, (</>))
import Test.Hspec
import TestUtils (fixturesBasePath)
import TypeScript.Config (ImportAlias (..), PathMapping (..), Pattern (..), TsConfigLegacy (..), parsePathMapping, parsePattern, parseTsConfigLegacy)

tsConfigFixturesPath :: OsPath
tsConfigFixturesPath = fixturesBasePath </> [osp|typescript|]

spec :: Spec
spec = describe "TsConfig" $ do
    describe "parsePathMapping" $ do
        it "fails if the target array is entirely empty" $ do
            parsePathMapping ("@app/*", []) `shouldBe` Nothing

        it "fails if the key contains an invalid pattern" $ do
            -- Even if targets are valid, a bad key ruins the mapping
            parsePathMapping ("@app/*/*", ["./src/*"]) `shouldBe` Nothing

        it "fails if all target paths are invalid patterns" $ do
            -- The mapMaybe drops all elements, making nonEmpty return Nothing
            parsePathMapping ("@core/*", ["./src/*/*", "./lib/*/*"]) `shouldBe` Nothing

        it "filters out invalid targets but succeeds if at least one is valid" $ do
            -- Mathematically, it should reduce the target set V to only valid elements
            parsePathMapping ("@app/*", ["./src/*", "./invalid/*/*", "./lib/*"])
                `shouldBe` Just
                    PathMapping
                        { key = Wildcard "@app/" ""
                        , values = Wildcard "./src/" "" :| [Wildcard "./lib/" ""]
                        }

        it "parses exact string mappings (no wildcards)" $ do
            parsePathMapping ("jquery", ["./vendor/jquery.js"])
                `shouldBe` Just
                    PathMapping
                        { key = Exact "jquery"
                        , values = Exact "./vendor/jquery.js" :| []
                        }

        it "parses wildcard mappings with multiple fallback targets" $ do
            parsePathMapping ("~/*-types", ["./src/types/*", "./shared/types/*-types.d.ts"])
                `shouldBe` Just
                    PathMapping
                        { key = Wildcard "~/" "-types"
                        , values = Wildcard "./src/types/" "" :| [Wildcard "./shared/types/" "-types.d.ts"]
                        }

        it "parses wildcard keys mapped to exact targets (valid in TS)" $ do
            parsePathMapping ("*.css", ["./src/mocks/style-mock.ts"])
                `shouldBe` Just
                    PathMapping
                        { key = Wildcard "" ".css"
                        , values = Exact "./src/mocks/style-mock.ts" :| []
                        }

        describe "parsePattern" $ do
            it "empty text is invalid" $ do
                parsePattern "" `shouldBe` Nothing

            it "exact paths (no wildcards)" $ do
                parsePattern "hello" `shouldBe` Just (Exact "hello")
                parsePattern "ui/component" `shouldBe` Just (Exact "ui/component")
                parsePattern "@angular/core/testing" `shouldBe` Just (Exact "@angular/core/testing")
                parsePattern ".hidden-dir/index.js" `shouldBe` Just (Exact ".hidden-dir/index.js")

            it "bare catch-all wildcard" $ do
                parsePattern "*" `shouldBe` Just (Wildcard "" "")

            it "wildcard at the end (prefix matching)" $ do
                parsePattern "@/*" `shouldBe` Just (Wildcard "@/" "")
                parsePattern "./*" `shouldBe` Just (Wildcard "./" "")
                parsePattern "src/*" `shouldBe` Just (Wildcard "src/" "")
                parsePattern "utils*" `shouldBe` Just (Wildcard "utils" "")

            it "wildcard at the beginning (suffix matching)" $ do
                parsePattern "*.spec.ts" `shouldBe` Just (Wildcard "" ".spec.ts")
                parsePattern "*-user" `shouldBe` Just (Wildcard "" "-user")

            it "wildcard in the middle (infix matching)" $ do
                parsePattern "@/data/*-dto" `shouldBe` Just (Wildcard "@/data/" "-dto")
                parsePattern "~/*/types" `shouldBe` Just (Wildcard "~/" "/types")

            it "invalid: more than one wildcard" $ do
                parsePattern "src/*/*" `shouldBe` Nothing
                parsePattern "**" `shouldBe` Nothing
                parsePattern "a*b*c" `shouldBe` Nothing
                parsePattern "*/utils/*" `shouldBe` Nothing

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
