{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolverSpec (spec) where

import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, runMockRoFileSystem)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (absPathUnsafe)
import System.OsPath (osp)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import TypeScript.ModuleResolver (Match (..), ModuleId (..), isRelativeImport, match, resolve, reverseResolve, reverseResolveImport)

spec :: Spec
spec = describe "ModuleResolver" $ do
    describe "match TSConfig pattern" $ do
        it "exact matching" $ do
            let pattern = Exact "react"
            -- Standard exact match
            match pattern "react" `shouldBe` Just ExactMatch
            -- Fails on partial match / extended string
            match pattern "react-dom" `shouldBe` Nothing
            -- Fails on case sensitivity difference
            match pattern "React" `shouldBe` Nothing
            -- Fails on completely unrelated string
            match (Exact "Hi") "Hello" `shouldBe` Nothing
            -- Fails on empty string
            match pattern "" `shouldBe` Nothing

        it "suffix wildcard (something/*)" $ do
            let pattern = Wildcard "src/" ""
            -- Succeeds with empty capture because '*' can match zero characters
            match pattern "src/" `shouldBe` Just (WildcardMatch "")
            -- Succeeds with a standard file
            match pattern "src/page.tsx" `shouldBe` Just (WildcardMatch "page.tsx")
            -- Succeeds with a deeply nested path
            match pattern "src/components/ui/button.tsx" `shouldBe` Just (WildcardMatch "components/ui/button.tsx")
            -- Fails on entirely wrong prefix
            match pattern "test/util.ts" `shouldBe` Nothing
            -- Fails on a partial/incomplete prefix match
            match pattern "sr/page.tsx" `shouldBe` Nothing
            -- Fails on empty string
            match pattern "" `shouldBe` Nothing

        it "infix wildcard (some/*-thing)" $ do
            let pattern = Wildcard "@types/" "-dto"
            -- Succeeds with empty capture because '*' can match zero characters
            match pattern "@types/-dto" `shouldBe` Just (WildcardMatch "")
            -- Succeeds with standard single-word wildcard match
            match pattern "@types/a-dto" `shouldBe` Just (WildcardMatch "a")
            -- Succeeds with nested paths caught in the wildcard section
            match pattern "@types/user-reg/old/something-dto" `shouldBe` Just (WildcardMatch "user-reg/old/something")
            -- Fails because the suffix has extra characters
            match pattern "@types/a-dtoS" `shouldBe` Nothing
            -- Fails because of file extension in suffix when not expected
            match pattern "@types/a-dto.ts" `shouldBe` Nothing
            -- Fails because the prefix does not match
            match pattern "@lib/a-dto" `shouldBe` Nothing
            -- Fails on empty string
            match pattern "" `shouldBe` Nothing

        it "prefix wildcard (*-something)" $ do
            let pattern = Wildcard "" "-spec.ts"
            -- Succeeds with empty capture because '*' can match zero characters
            match pattern "-spec.ts" `shouldBe` Just (WildcardMatch "")
            -- Succeeds because the wildcard matches a standard word
            match pattern "auth-spec.ts" `shouldBe` Just (WildcardMatch "auth")
            -- Succeeds because the wildcard matches a nested directory path
            match pattern "src/components/button-spec.ts" `shouldBe` Just (WildcardMatch "src/components/button")
            -- Fails because the suffix does not match exactly (extra 'x')
            match pattern "auth-spec.tsx" `shouldBe` Nothing
            -- Fails because it lacks the required suffix entirely
            match pattern "auth.ts" `shouldBe` Nothing
            -- Fails on empty string
            match pattern "" `shouldBe` Nothing

    describe "reverseResolve (Reverse Path Resolution)" $ do
        let dummyBaseUrl = absPathUnsafe [osp|/home/repo|]
        let baseCfg = TsConfig {baseUrl = dummyBaseUrl, paths = []}

        let mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)

        let runEncodeTest cfg pathStr =
                let path = absPathUnsafe pathStr
                 in runPureEff
                        . runReader cfg
                        $ reverseResolve path

        it "resolves relative to baseUrl when there are no path mappings" $ do
            let result = runEncodeTest baseCfg [osp|/home/repo/src/lib/util.tsx|]
            result `shouldBe` (Just $ ModuleId "src/lib/util")

        it "resolves relative to baseUrl when mappings exist but do not match" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/test/util.ts|]
            result `shouldBe` (Just $ ModuleId "test/util")

        it "applies an Exact path mapping" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Exact "jquery") [Exact "node_modules/jquery/dist/jquery"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/node_modules/jquery/dist/jquery.js|]
            result `shouldBe` (Just $ ModuleId "jquery")

        it "applies a Suffix Wildcard path mapping" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/lib/util.tsx|]
            result `shouldBe` (Just $ ModuleId "@/lib/util")

        it "applies an Infix Wildcard path mapping" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@dto/" "-dto") [Wildcard "src/types/" "-dto"]]
                        }
            let resMatch = runEncodeTest cfg [osp|/home/repo/src/types/user/account-dto.ts|]
            resMatch `shouldBe` (Just $ ModuleId "@dto/user/account-dto")
            let resNotFound = runEncodeTest cfg [osp|/home/repo/src/types/user/account.ts|]
            resNotFound `shouldBe` (Just $ ModuleId "src/types/user/account")

        it "handles prefix wildcards (*-spec)" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@tests/" "-spec") [Wildcard "src/tests/" "-spec"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/src/tests/auth-spec.ts|]
            result `shouldBe` (Just $ ModuleId "@tests/auth-spec")

        it "handles fallback values in mapping array (matches the second value)" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping
                                (Wildcard "@utils/" "")
                                [ Wildcard "src/utils/" ""
                                , Wildcard "shared/utils/" ""
                                ]
                            ]
                        }
            -- Matches the second value "shared/utils/*"
            let result = runEncodeTest cfg [osp|/home/repo/shared/utils/math.ts|]
            result `shouldBe` (Just $ ModuleId "@utils/math")

        it "picks the first matched mapping (ensures correct priority execution)" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping (Exact "@utils/math") [Exact "src/utils/math"]
                            , mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]
                            ]
                        }
            -- Even though "src/utils/*" would match, the exact match is listed first.
            let result = runEncodeTest cfg [osp|/home/repo/src/utils/math.ts|]
            result `shouldBe` (Just $ ModuleId "@utils/math")

        it "recovers via fall-through if an invalid Exact-Key to Wildcard-Value match is encountered" $ do
            let cfg =
                    baseCfg
                        { paths =
                            -- The first mapping is technically invalid TS (Exact to Wildcard)
                            [ mkMapping (Exact "invalid-exact") [Wildcard "src/libs/" ""]
                            , mkMapping (Wildcard "@libs/" "") [Wildcard "src/libs/" ""]
                            ]
                        }
            -- It should hit the first mapping, realize it can't apply a capture to an Exact key,
            -- safely fall through, and successfully match the second mapping.
            let result = runEncodeTest cfg [osp|/home/repo/src/libs/logger.ts|]
            result `shouldBe` (Just $ ModuleId "@libs/logger")

        it "handles Wildcard keys mapped to Exact values" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@core/" "") [Exact "src/core"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/src/core.ts|]
            -- Candidate "src/core" matches Exact "src/core" -> ExactMatch
            -- Applying ExactMatch to Wildcard "@core/" "" -> "@core/" <> "" <> "" -> "@core/"
            result `shouldBe` (Just $ ModuleId "@core/")

    describe "isRelativeImport" $ do
        it "identifies strict current directory (.)" $ do
            isRelativeImport (ModuleId ".") `shouldBe` True

        it "identifies strict parent directory (..)" $ do
            isRelativeImport (ModuleId "..") `shouldBe` True

        it "identifies current directory prefix (./)" $ do
            isRelativeImport (ModuleId "./") `shouldBe` True
            isRelativeImport (ModuleId "./components/Button") `shouldBe` True

        it "identifies parent directory prefix (../)" $ do
            isRelativeImport (ModuleId "../") `shouldBe` True
            isRelativeImport (ModuleId "../utils/math") `shouldBe` True
            isRelativeImport (ModuleId "../../shared/types") `shouldBe` True

        it "identifies root/absolute paths (/)" $ do
            -- Note: TypeScript treats absolute paths as "relative" module resolutions
            -- because they bypass TSConfig mappings and node_modules lookup.
            isRelativeImport (ModuleId "/") `shouldBe` True
            isRelativeImport (ModuleId "/home/repo/src/main") `shouldBe` True

        it "rejects non-relative bare specifiers" $ do
            isRelativeImport (ModuleId "react") `shouldBe` False
            isRelativeImport (ModuleId "lodash/fp") `shouldBe` False
            isRelativeImport (ModuleId "src/utils/math") `shouldBe` False

        it "rejects non-relative aliased specifiers" $ do
            isRelativeImport (ModuleId "@utils/math") `shouldBe` False
            isRelativeImport (ModuleId "@/components/Button") `shouldBe` False

        it "rejects specifiers that start with dots but lack slashes (TS edge cases)" $ do
            isRelativeImport (ModuleId ".hidden-module") `shouldBe` False
            isRelativeImport (ModuleId "..double-dot-module") `shouldBe` False
            isRelativeImport (ModuleId "...") `shouldBe` False

        it "rejects inner-relative paths (must start with relative prefix)" $ do
            isRelativeImport (ModuleId "utils/../math") `shouldBe` False

    describe "resolve (Forward Path Resolution)" $ do
        let dummyBaseUrl = absPathUnsafe [osp|/home/repo|]
        let baseCfg = TsConfig {baseUrl = dummyBaseUrl, paths = []}

        let mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)

        -- Helper to run the resolve function from a specific importing file
        let runResolveTestFrom importerAbsPath cfg existingFiles mId =
                let mockFs =
                        defaultMockRoFileSystem
                            { mockFileExistsAbs = \p -> pure $ p `elem` existingFiles
                            }
                 in runPureEff
                        . runMockRoFileSystem mockFs
                        . runReader cfg
                        $ resolve importerAbsPath (ModuleId mId)

        -- Default helper for non-relative tests to avoid rewriting existing cases
        let runResolveTest = runResolveTestFrom (absPathUnsafe [osp|/home/repo/src/main.ts|])

        it "resolves relative to baseUrl with a .ts extension" $ do
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/lib/util.ts|]]
            let result = runResolveTest baseCfg existingFiles "src/lib/util"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/util.ts|]

        it "resolves relative to baseUrl with a .tsx extension" $ do
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/lib/util.tsx|]]
            let result = runResolveTest baseCfg existingFiles "src/lib/util"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/util.tsx|]

        it "resolves relative to baseUrl using an index.ts file (Directory fallback)" $ do
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/lib/util/index.ts|]]
            let result = runResolveTest baseCfg existingFiles "src/lib/util"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/util/index.ts|]

        it "resolves relative to baseUrl using an index.tsx file" $ do
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/components/Button/index.tsx|]]
            let result = runResolveTest baseCfg existingFiles "src/components/Button"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/components/Button/index.tsx|]

        it "respects TypeScript's extension probing priority (.ts > .tsx > index.ts > index.tsx)" $ do
            let existingFiles =
                    [ absPathUnsafe [osp|/home/repo/src/components/Button.tsx|]
                    , absPathUnsafe [osp|/home/repo/src/components/Button.ts|]
                    , -- Should win
                      absPathUnsafe [osp|/home/repo/src/components/Button/index.ts|]
                    ]
            let result = runResolveTest baseCfg existingFiles "src/components/Button"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/components/Button.ts|]

        it "resolves an Exact mapping to a .ts file" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Exact "jquery") [Exact "node_modules/jquery/dist/jquery"]]
                        }
            let existingFiles = [absPathUnsafe [osp|/home/repo/node_modules/jquery/dist/jquery.ts|]]
            let result = runResolveTest cfg existingFiles "jquery"
            result `shouldBe` absPathUnsafe [osp|/home/repo/node_modules/jquery/dist/jquery.ts|]

        it "resolves a Wildcard suffix mapping to a .tsx file" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/components/Button.tsx|]]
            let result = runResolveTest cfg existingFiles "@/components/Button"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/components/Button.tsx|]

        it "resolves an Infix wildcard mapping to an index.ts file" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@dto/" "-dto") [Wildcard "src/types/" "-dto"]]
                        }
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/types/user/account-dto/index.ts|]]
            let result = runResolveTest cfg existingFiles "@dto/user/account-dto"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/types/user/account-dto/index.ts|]

        it "handles fallback values in the mapping array (first fails, second succeeds)" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping
                                (Wildcard "@utils/" "")
                                [ Wildcard "src/utils/" ""
                                , Wildcard "shared/utils/" ""
                                ]
                            ]
                        }
            -- The algorithm checks 'src/utils/math' variations, fails, and falls back to 'shared/utils'
            let existingFiles = [absPathUnsafe [osp|/home/repo/shared/utils/math.ts|]]
            let result = runResolveTest cfg existingFiles "@utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/shared/utils/math.ts|]

        it "handles fallback values finding an index file on the second array entry" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping
                                (Wildcard "@utils/" "")
                                [ Wildcard "src/utils/" ""
                                , Wildcard "shared/utils/" ""
                                ]
                            ]
                        }
            let existingFiles = [absPathUnsafe [osp|/home/repo/shared/utils/math/index.tsx|]]
            let result = runResolveTest cfg existingFiles "@utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/shared/utils/math/index.tsx|]

        it "falls through to the next mapping if all fallback values in the first mapping fail" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]
                            , mkMapping (Wildcard "@utils/" "") [Wildcard "fallback/utils/" ""]
                            ]
                        }
            let existingFiles = [absPathUnsafe [osp|/home/repo/fallback/utils/math.ts|]]
            let result = runResolveTest cfg existingFiles "@utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/fallback/utils/math.ts|]

        it "respects exact mappings over wildcard mappings if matched first" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping (Exact "@utils/math") [Exact "src/special/math"]
                            , mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]
                            ]
                        }
            let existingFiles =
                    [ absPathUnsafe [osp|/home/repo/src/special/math.ts|]
                    , absPathUnsafe [osp|/home/repo/src/utils/math.ts|]
                    ]
            let result = runResolveTest cfg existingFiles "@utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/special/math.ts|]

        it "handles an empty capture (root directory import) resolving to an index file" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            -- Importing "@utils/" results in an empty string capture.
            -- It should append the capture and test "src/utils/.ts" (fails) then "src/utils//index.ts" (succeeds).
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/utils/index.ts|]]

            let result = runResolveTest cfg existingFiles "@utils/"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/utils/index.ts|]

        it "maps a wildcard key to an exact value (ignoring the captured string)" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@core/" "") [Exact "src/core-singleton"]]
                        }
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/core-singleton.ts|]]

            -- Even though the capture is "feature/deep/path", the exact value discards it.
            let result = runResolveTest cfg existingFiles "@core/feature/deep/path"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/core-singleton.ts|]

        it "returns the raw baseUrl path if absolutely no probed extensions exist on disk" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            -- The mock file system is completely empty
            let existingFiles = []

            -- It should fail the path mapping, hit the `Nothing` branch,
            -- fail all `baseUrl` extensions, and finally return the raw absolute path.
            let result = runResolveTest cfg existingFiles "@/missing/module"
            result `shouldBe` absPathUnsafe [osp|/home/repo/@/missing/module|]

        it "resolves a same-directory relative import (./) with extension probing" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/pages/LoginView.tsx|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "./LoginView"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/pages/LoginView.tsx|]

        it "resolves a parent-directory relative import (../)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/utils/math.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "../utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/utils/math.ts|]

        it "resolves current directory root (.) to an index file" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/pages/index.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "."
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/pages/index.ts|]

        it "resolves parent directory root (..) to an index file" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/index.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles ".."
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/index.ts|]

        it "resolves multi-level parent directory relative imports (../../)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/dashboard/User.tsx|]
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/lib/api.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "../../lib/api"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/api.ts|]

        it "strictly bypasses TSConfig path mappings for relative imports" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "./utils/" "") [Wildcard "src/hacked/utils/" ""]]
                        }
            -- Even though a path mapping matches the prefix exactly, TS ignores it completely
            -- because relative paths are tightly bound to the disk, never the compiler mappings.
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/pages/utils/math.ts|]]

            let result = runResolveTestFrom importer cfg existingFiles "./utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/pages/utils/math.ts|]

    describe "reverseResolveImport" $ do
        let dummyBaseUrl = absPathUnsafe [osp|/home/repo|]
        let baseCfg = TsConfig {baseUrl = dummyBaseUrl, paths = []}

        let mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)

        let runRRTest importerAbsPath cfg existingFiles mIdStr =
                let mockFs =
                        defaultMockRoFileSystem
                            { mockFileExistsAbs = \p -> pure $ p `elem` existingFiles
                            }
                 in runPureEff
                        . runMockRoFileSystem mockFs
                        . runReader cfg
                        $ reverseResolveImport importerAbsPath (ModuleId mIdStr)

        it "converts a parent-directory relative import to an aliased import if a mapping exists" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/utils/math.ts|]]

            let result = runRRTest importer cfg existingFiles "../utils/math"
            result `shouldBe` ModuleId "@utils/math"

        it "converts a same-directory relative import to an aliased import if a mapping exists" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@pages/" "") [Wildcard "src/pages/" ""]]}
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/pages/LoginView.tsx|]]

            let result = runRRTest importer cfg existingFiles "./LoginView"
            result `shouldBe` ModuleId "@pages/LoginView"

        it "converts a relative import to a baseUrl-relative absolute import if no path mapping exists (inside baseUrl)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/utils/math.ts|]]

            -- By TS rules, if there's no alias but it's in the baseUrl, it's valid to make it a bare specifier.
            let result = runRRTest importer baseCfg existingFiles "../utils/math"
            result `shouldBe` ModuleId "src/utils/math"

        it "improves an existing aliased import if a more specific/shorter alias matches" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg =
                    baseCfg
                        { paths =
                            -- Assume @ui/ is preferred or listed first in the resolved config
                            [ mkMapping (Wildcard "@ui/" "") [Wildcard "src/components/ui/" ""]
                            , mkMapping (Wildcard "@components/" "") [Wildcard "src/components/" ""]
                            ]
                        }
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/components/ui/button.tsx|]]

            -- Original import used the broader `@components/` alias
            let result = runRRTest importer cfg existingFiles "@components/ui/button"
            -- It should upgrade to the more specific `@ui/` alias
            result `shouldBe` ModuleId "@ui/button"

        it "leaves an aliased import as-is if it is already the optimal choice" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/utils/math.ts|]]

            let result = runRRTest importer cfg existingFiles "@utils/math"
            result `shouldBe` ModuleId "@utils/math"

        it "leaves non-relative bare module specifiers (node_modules) as-is" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            let existingFiles = []

            -- `resolve` might fail to find external modules in local pure fs,
            -- but the reverse resolver should gracefully leave the raw target untouched.
            let result = runRRTest importer cfg existingFiles "react"
            result `shouldBe` ModuleId "react"

        it "correctly resolves a relative import pointing to a directory index to its aliased equivalent" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            let existingFiles = [absPathUnsafe [osp|/home/repo/src/utils/index.ts|]]

            -- The relative import resolves to /home/repo/src/utils/index.ts
            -- The alias engine should map that back to `@utils/index` or `@utils/`
            let result = runRRTest importer cfg existingFiles "../utils"
            result `shouldBe` ModuleId "@utils/index"

        it "leaves relative imports pointing entirely outside the baseUrl as-is" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}

            -- Importer is in /home/repo/src/pages/
            -- ../../../ -> /home/
            -- Target is /home/shared/types.ts
            let existingFiles = [absPathUnsafe [osp|/home/shared/types.ts|]]

            let result = runRRTest importer cfg existingFiles "../../../shared/types"
            result `shouldBe` ModuleId "../../../shared/types"

        it "converts an outside-baseUrl relative import to an aliased import if an explicit mapping exists for it" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@shared/" "") [Wildcard "../shared/" ""]]
                        }
            -- Aliases can map out of the baseUrl via "../"
            let existingFiles = [absPathUnsafe [osp|/home/shared/utils.ts|]]

            let result = runRRTest importer cfg existingFiles "../../../shared/utils"
            result `shouldBe` ModuleId "@shared/utils"

        it "leaves outside-baseUrl relative imports as-is even if they share folder names with inside-baseUrl paths" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = []}

            -- Target is /home/src/utils.ts (Outside baseUrl, but shares 'src' name)
            let existingFiles = [absPathUnsafe [osp|/home/src/utils.ts|]]

            -- Should NOT resolve to "src/utils" because it's not the /home/repo/src/utils
            let result = runRRTest importer cfg existingFiles "../../../src/utils"
            result `shouldBe` ModuleId "../../../src/utils"
