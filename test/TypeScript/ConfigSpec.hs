module TypeScript.ConfigSpec (spec) where

import Data.Text qualified as T
import Effectful (runEff)
import Effects.FileSystem (encodeOsPath, runFileSystemIO)
import System.OsPath (osp, (</>))
import Test.Hspec
import TestUtils (mkAbsolute, pathSafeGolden)
import Text.Show.Pretty (ppShow)
import TypeScript.Config (PathMapping (..), Pattern (..), parsePathMapping, parsePattern, readTsConfig)

spec :: Spec
spec = describe "TsConfig" $ do
    describe "parsePathMapping" $ do
        it "fails if the target array is entirely empty" $ do
            parsePathMapping ("@app/*", []) `shouldBe` Nothing

        it "fails if the key contains an invalid pattern" $ do
            -- Even if targets are valid, a bad key ruins the mapping
            parsePathMapping ("@app/*/*", ["./src/*"]) `shouldBe` Nothing

        it "fails if all target paths are invalid patterns" $ do
            parsePathMapping ("@core/*", ["./src/*/*", "./lib/*/*"]) `shouldBe` Nothing

        it "filters out invalid targets but succeeds if at least one is valid" $ do
            -- It should reduce the target set V to only valid elements
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

    describe "readTsConfig from file (E2E)" $ do
        let cases =
                [ "tsconfig-simple.json"
                , "tsconfig-invalid.json"
                , "tsconfig-complex.json"
                ]
        forM_ cases $ \file ->
            it file $ do
                cfgPath <- mkAbsolute ([osp|test/fixtures/typescript|] </> (encodeOsPath $ T.pack file))
                res <- runEff . runFileSystemIO $ readTsConfig cfgPath
                pathSafeGolden ("readTsConfig-" <> file) (ppShow res)
