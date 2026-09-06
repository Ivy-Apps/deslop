module TypeScript.ConfigSpec (spec) where

import FileSystem.Path (AbsPath)
import Fixtures.TypeScript.Config (mkMapping)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (ap, prop)
import TypeScript.Config (
    Declared (..),
    DeclaredPaths (..),
    KeyPattern (..),
    PathMapping (..),
    Pattern (..),
    TsConfig (..),
    ValuePattern (..),
    effectiveConfig,
    parsePathMapping,
    parsePattern,
 )

spec :: Spec
spec = describe "TypeScript.Config" $ do
    describe "parsePathMapping" $ do
        it "fails if the target array is entirely empty" $ do
            parsePathMapping ("@app/*", []) `shouldBe` Nothing

        it "fails if the key contains an invalid pattern" $ do
            -- Even if targets are valid, a bad key ruins the mapping
            parsePathMapping ("@app/*/*", ["./src/*"]) `shouldBe` Nothing

        it "fails if all target paths are invalid patterns" $ do
            parsePathMapping ("@core/*", ["./src/*/*", "./lib/*/*"]) `shouldBe` Nothing

        it "filters out invalid targets, succeeds if at least one is valid, and strips './' prefixes" $ do
            -- It should reduce the target set V to only valid elements and clean the prefixes
            parsePathMapping ("@app/*", ["./src/*", "./invalid/*/*", "./lib/*"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Wildcard "@app/" ""
                        , values = fmap ValuePattern $ Wildcard "src/" "" :| [Wildcard "lib/" ""]
                        }

        it "parses exact string mappings and cleans meaningless './' prefixes" $ do
            parsePathMapping ("jquery", ["./vendor/jquery.js"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Exact "jquery"
                        , values = fmap ValuePattern $ Exact "vendor/jquery.js" :| []
                        }

        it "parses wildcard mappings with multiple fallback targets and cleans prefixes" $ do
            parsePathMapping ("~/*-types", ["./src/types/*", "./shared/types/*-types.d.ts"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Wildcard "~/" "-types"
                        , values =
                            fmap ValuePattern $
                                Wildcard "src/types/" "" :| [Wildcard "shared/types/" "-types.d.ts"]
                        }

        it "parses wildcard keys mapped to exact targets and cleans prefixes" $ do
            parsePathMapping ("*.css", ["./src/mocks/style-mock.ts"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Wildcard "" ".css"
                        , values = fmap ValuePattern $ Exact "src/mocks/style-mock.ts" :| []
                        }

        it "fails if key is Exact but the values are Wildcard" $ do
            parsePathMapping ("react", ["*.css"]) `shouldBe` Nothing

        it "cleans a Next.js root alias mapping ('./*') into a clean catch-all wildcard ('*')" $ do
            parsePathMapping ("@/*", ["./*"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Wildcard "@/" ""
                        , -- The "./" is stripped, leaving an empty prefix before the wildcard
                          values = fmap ValuePattern $ Wildcard "" "" :| []
                        }

        it "recursively cleans redundant nested current-directory prefixes ('./././')" $ do
            parsePathMapping ("@app/*", ["./././src/*", "././lib/*"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Wildcard "@app/" ""
                        , values = fmap ValuePattern $ Wildcard "src/" "" :| [Wildcard "lib/" ""]
                        }

        it "flattens a pure '.' exact mapping into an empty string to prevent floating segments" $ do
            parsePathMapping ("@root", ["."])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Exact "@root"
                        , values = fmap ValuePattern $ Exact "" :| []
                        }

        it "safely preserves valid parent directory traversals ('../') in alias targets" $ do
            parsePathMapping ("@shared/*", ["../shared/*", "./../external/*"])
                `shouldBe` Just
                    PathMapping
                        { key = KeyPattern $ Wildcard "@shared/" ""
                        , -- The leading "./" on the second target is cleaned, but both retain "../"
                          values = fmap ValuePattern $ Wildcard "../shared/" "" :| [Wildcard "../external/" ""]
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

    describe "effectiveConfig" $ do
        it "resolves paths against a declared baseUrl rather than the declaring file's directory" $ do
            let declared = declaredBaseUrl "/repo/src" <> declaredPaths "/repo/config"

            effectiveConfig (ap "/repo") declared
                `shouldBe` TsConfig {pathsBase = ap "/repo/src", paths = [aliasMapping]}

        it "falls back to the directory of the config that declared the paths" $ do
            let declared = declaredPaths "/repo/config"

            effectiveConfig (ap "/repo") declared
                `shouldBe` TsConfig {pathsBase = ap "/repo/config", paths = [aliasMapping]}

        it "falls back to the root config's directory when nothing was declared" $ do
            effectiveConfig (ap "/repo") mempty
                `shouldBe` TsConfig {pathsBase = ap "/repo", paths = []}

        it "keeps a declared baseUrl even when no config declared any paths" $ do
            effectiveConfig (ap "/repo") (declaredBaseUrl "/repo/src")
                `shouldBe` TsConfig {pathsBase = ap "/repo/src", paths = []}

        it "lets a config's paths replace, never union, the ones it extends" $ do
            let base = Declared {baseUrl = mempty, paths = pure (DeclaredPaths (ap "/repo") [otherMapping])}
            let child = declaredPaths "/repo"

            (effectiveConfig (ap "/repo") (base <> child)).paths `shouldBe` [aliasMapping]

    describe "merging what each config declares" $ do
        prop "is associative" $ do
            (a, b, c) <- forAll $ (,,) <$> genDeclared <*> genDeclared <*> genDeclared

            (a <> b) <> c === a <> (b <> c)

        prop "leaves a config that declares nothing without effect" $ do
            declared <- forAll genDeclared

            (mempty <> declared, declared <> mempty) === (declared, declared)

        prop "gives every option to its last declaration" $ do
            declareds <- forAll . Gen.list (Range.linear 1 8) $ genDeclared

            let merged = fold declareds
            ( getLast merged.baseUrl
                , getLast merged.paths
                )
                === ( lastDeclared (getLast . (.baseUrl) <$> declareds)
                    , lastDeclared (getLast . (.paths) <$> declareds)
                    )

        prop "ignores the root directory whenever anything declared a base" $ do
            declared <- forAll genDeclaringSomething
            (rootDir, otherRootDir) <- forAll $ (,) <$> genAbsPath <*> genAbsPath

            effectiveConfig rootDir declared === effectiveConfig otherRootDir declared

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genDeclared :: Gen Declared
genDeclared =
    Declared
        <$> fmap Last (Gen.maybe genAbsPath)
        <*> fmap Last (Gen.maybe genDeclaredPaths)

-- | A config that declares at least one of the two options, so the fallback cannot apply.
genDeclaringSomething :: Gen Declared
genDeclaringSomething = Gen.filter declaresSomething genDeclared
  where
    declaresSomething d = isJust (getLast d.baseUrl) || isJust (getLast d.paths)

genDeclaredPaths :: Gen DeclaredPaths
genDeclaredPaths =
    DeclaredPaths
        <$> genAbsPath
        <*> Gen.subsequence [aliasMapping, otherMapping]

-- | Distinct enough to tell apart, and never the directory a fallback would pick.
genAbsPath :: Gen AbsPath
genAbsPath = ap . ("/declared/" <>) <$> Gen.element ["a", "b", "c", "d"]

lastDeclared :: [Maybe a] -> Maybe a
lastDeclared = listToMaybe . reverse . catMaybes

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

declaredBaseUrl :: Text -> Declared
declaredBaseUrl dir = Declared {baseUrl = pure (ap dir), paths = mempty}

declaredPaths :: Text -> Declared
declaredPaths dir =
    Declared {baseUrl = mempty, paths = pure (DeclaredPaths (ap dir) [aliasMapping])}

aliasMapping :: PathMapping
aliasMapping = mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]

otherMapping :: PathMapping
otherMapping = mkMapping (Wildcard "~/" "") [Wildcard "lib/" ""]
