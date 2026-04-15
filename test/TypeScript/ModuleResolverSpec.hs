{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolverSpec (spec) where

import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (absPathUnsafe)
import System.OsPath (osp)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (mkMapping)
import TypeScript.Config (Pattern (..), TsConfig (..))
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

        let runEncodeTest cfg pathStr =
                let path = absPathUnsafe pathStr
                 in runPureEff
                        . runReader cfg
                        $ reverseResolve path

        it "resolves relative to baseUrl when there are no path mappings" $ do
            let result = runEncodeTest baseCfg [osp|/home/repo/src/lib/util.tsx|]
            result `shouldBe` Just (ModuleId "src/lib/util")

        it "resolves relative to baseUrl when mappings exist but do not match" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/test/util.ts|]
            result `shouldBe` Just (ModuleId "test/util")

        it "applies an Exact path mapping" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Exact "jquery") [Exact "node_modules/jquery/dist/jquery"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/node_modules/jquery/dist/jquery.js|]
            result `shouldBe` Just (ModuleId "jquery")

        it "applies a Suffix Wildcard path mapping" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/lib/util.tsx|]
            result `shouldBe` Just (ModuleId "@/lib/util")

        it "applies an Infix Wildcard path mapping" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@dto/" "-dto") [Wildcard "src/types/" "-dto"]]
                        }
            let resMatch = runEncodeTest cfg [osp|/home/repo/src/types/user/account-dto.ts|]
            resMatch `shouldBe` Just (ModuleId "@dto/user/account-dto")
            let resNotFound = runEncodeTest cfg [osp|/home/repo/src/types/user/account.ts|]
            resNotFound `shouldBe` Just (ModuleId "src/types/user/account")

        it "handles prefix wildcards (*-spec)" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@tests/" "-spec") [Wildcard "src/tests/" "-spec"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/src/tests/auth-spec.ts|]
            result `shouldBe` Just (ModuleId "@tests/auth-spec")

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
            result `shouldBe` Just (ModuleId "@utils/math")

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
            result `shouldBe` Just (ModuleId "@utils/math")

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
            result `shouldBe` Just (ModuleId "@libs/logger")

        it "handles Wildcard keys mapped to Exact values" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@core/" "") [Exact "src/core"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/src/core.ts|]
            -- Candidate "src/core" matches Exact "src/core" -> ExactMatch
            -- Applying ExactMatch to Wildcard "@core/" "" -> "@core/" <> "" <> "" -> "@core/"
            result `shouldBe` Just (ModuleId "@core/")

        it "returns Nothing when target is outside baseUrl and no mappings exist" $ do
            let cfg = baseCfg {paths = []}
            -- Target is physically outside /home/repo/
            let result = runEncodeTest cfg [osp|/home/shared/utils.ts|]

            -- Because it's outside and unmapped, it cannot be a bare specifier.
            result `shouldBe` Nothing

        it "returns Nothing when target is outside baseUrl and existing mappings do not match" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            -- Target is physically outside /home/repo/
            let result = runEncodeTest cfg [osp|/home/external/api.ts|]

            result `shouldBe` Nothing

        it "returns Nothing for global system paths (e.g., global node_modules or standard libs)" $ do
            let cfg = baseCfg {paths = []}
            -- Target is a completely divergent absolute path
            let result = runEncodeTest cfg [osp|/usr/local/lib/node_modules/react/index.js|]

            result `shouldBe` Nothing

        it "returns Nothing for a monorepo sibling package that is not mapped in TSConfig" $ do
            -- Imagine baseUrl is deeply nested in a workspace
            let webBaseUrl = absPathUnsafe [osp|/home/repo/packages/web|]
            let webCfg = TsConfig {baseUrl = webBaseUrl, paths = []}

            -- Importing from a sibling package
            let result = runEncodeTest webCfg [osp|/home/repo/packages/ui/button.tsx|]

            result `shouldBe` Nothing

        it "resolves a Vite-style alias with a custom symbol ($)" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "$utils/" "") [Wildcard "src/utils/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/utils/formatter.ts|]
            result `shouldBe` Just (ModuleId "$utils/formatter")

        it "resolves to the first available mapping in a multi-value fallback array" $ do
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping
                                (Wildcard "@lib/" "")
                                [ Wildcard "src/lib/" ""
                                , Wildcard "shared/lib/" ""
                                ]
                            ]
                        }
            -- If the file is in the first target path
            let result = runEncodeTest cfg [osp|/home/repo/src/lib/core.ts|]
            result `shouldBe` Just (ModuleId "@lib/core")

        it "resolves a shared assets folder alias common in React/Vite" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@assets/" "") [Wildcard "src/assets/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/assets/images/logo.png|]
            result `shouldBe` Just (ModuleId "@assets/images/logo.png")

        it "resolves a root-level config file using an exact alias" $ do
            let cfg = baseCfg {paths = [mkMapping (Exact "config") [Exact "constants/app-config"]]}
            let result = runEncodeTest cfg [osp|/home/repo/constants/app-config.ts|]
            result `shouldBe` Just (ModuleId "config")

        it "resolves a CSS module file retaining its full extension (.module.css)" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/components/Button.module.css|]
            result `shouldBe` Just (ModuleId "@/components/Button.module.css")

        it "resolves a directory index file to the directory name (clean index resolution)" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            -- Logical resolution of /index.ts should often prefer the directory name in modern stacks
            let result = runEncodeTest cfg [osp|/home/repo/src/lib/utils/index.ts|]
            result `shouldBe` Just (ModuleId "@/lib/utils/index")

        it "resolves a type definition file (.d.ts) by dropping the extension like a source file" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@types/" "") [Wildcard "src/types/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/types/user.d.ts|]
            result `shouldBe` Just (ModuleId "@types/user")

        it "resolves a file with multiple dots (e.g. .controller.ts) by dropping only the final extension" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@api/" "") [Wildcard "src/api/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/api/user.controller.ts|]
            result `shouldBe` Just (ModuleId "@api/user.controller")

        it "resolves a directory index.d.ts file to the clean directory alias" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@types/" "") [Wildcard "src/types/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/types/global/index.d.ts|]
            result `shouldBe` Just (ModuleId "@types/global/index")

        it "resolves modern TS extensions (.mts, .cts) by dropping the extension" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@lib/" "") [Wildcard "src/lib/" ""]]}
            -- NOTE: This will fail until you add .mts, .cts, .mjs, .cjs to `dropTypeScriptExtension`!
            let result = runEncodeTest cfg [osp|/home/repo/src/lib/math.mts|]
            result `shouldBe` Just (ModuleId "@lib/math")

        it "resolves modern TS double-extensions (.d.mts, .d.cts) by dropping both" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@types/" "") [Wildcard "src/types/" ""]]}
            -- NOTE: This will fail until you add .d.mts and .d.cts to `dropTypeScriptExtension`!
            let result = runEncodeTest cfg [osp|/home/repo/src/types/node.d.mts|]
            result `shouldBe` Just (ModuleId "@types/node")

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

        -- Helper to run the resolve function from a specific importing file
        let runResolveTestFrom importerAbsPath cfg existingFiles mId =
                runPureEff
                    . runMockRoFileSystem (mockFiles existingFiles)
                    . runReader cfg
                    $ resolve importerAbsPath (ModuleId mId)

        -- Default helper for non-relative tests to avoid rewriting existing cases
        let runResolveTest = runResolveTestFrom (absPathUnsafe [osp|/home/repo/src/main.ts|])

        it "resolves relative to baseUrl with a .ts extension" $ do
            let existingFiles = [[osp|/home/repo/src/lib/util.ts|]]
            let result = runResolveTest baseCfg existingFiles "src/lib/util"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/util.ts|]

        it "resolves relative to baseUrl with a .tsx extension" $ do
            let existingFiles = [[osp|/home/repo/src/lib/util.tsx|]]
            let result = runResolveTest baseCfg existingFiles "src/lib/util"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/util.tsx|]

        it "resolves relative to baseUrl using an index.ts file (Directory fallback)" $ do
            let existingFiles = [[osp|/home/repo/src/lib/util/index.ts|]]
            let result = runResolveTest baseCfg existingFiles "src/lib/util"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/lib/util/index.ts|]

        it "resolves relative to baseUrl using an index.tsx file" $ do
            let existingFiles = [[osp|/home/repo/src/components/Button/index.tsx|]]
            let result = runResolveTest baseCfg existingFiles "src/components/Button"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/components/Button/index.tsx|]

        it "respects TypeScript's extension probing priority (.ts > .tsx > index.ts > index.tsx)" $ do
            let existingFiles =
                    [ [osp|/home/repo/src/components/Button.tsx|]
                    , [osp|/home/repo/src/components/Button.ts|]
                    , -- Should win
                      [osp|/home/repo/src/components/Button/index.ts|]
                    ]
            let result = runResolveTest baseCfg existingFiles "src/components/Button"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/components/Button.ts|]

        it "resolves an Exact mapping to a .ts file" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Exact "jquery") [Exact "node_modules/jquery/dist/jquery"]]
                        }
            let existingFiles = [[osp|/home/repo/node_modules/jquery/dist/jquery.ts|]]
            let result = runResolveTest cfg existingFiles "jquery"
            result `shouldBe` absPathUnsafe [osp|/home/repo/node_modules/jquery/dist/jquery.ts|]

        it "resolves a Wildcard suffix mapping to a .tsx file" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let existingFiles = [[osp|/home/repo/src/components/Button.tsx|]]
            let result = runResolveTest cfg existingFiles "@/components/Button"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/components/Button.tsx|]

        it "resolves an Infix wildcard mapping to an index.ts file" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@dto/" "-dto") [Wildcard "src/types/" "-dto"]]
                        }
            let existingFiles = [[osp|/home/repo/src/types/user/account-dto/index.ts|]]
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
            let existingFiles = [[osp|/home/repo/shared/utils/math.ts|]]
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
            let existingFiles = [[osp|/home/repo/shared/utils/math/index.tsx|]]
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
            let existingFiles = [[osp|/home/repo/fallback/utils/math.ts|]]
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
                    [ [osp|/home/repo/src/special/math.ts|]
                    , [osp|/home/repo/src/utils/math.ts|]
                    ]
            let result = runResolveTest cfg existingFiles "@utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/special/math.ts|]

        it "handles an empty capture (root directory import) resolving to an index file" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            -- Importing "@utils/" results in an empty string capture.
            -- It should append the capture and test "src/utils/.ts" (fails) then "src/utils//index.ts" (succeeds).
            let existingFiles = [[osp|/home/repo/src/utils/index.ts|]]

            let result = runResolveTest cfg existingFiles "@utils/"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/utils/index.ts|]

        it "maps a wildcard key to an exact value (ignoring the captured string)" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@core/" "") [Exact "src/core-singleton"]]
                        }
            let existingFiles = [[osp|/home/repo/src/core-singleton.ts|]]

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
            let existingFiles = [[osp|/home/repo/src/pages/LoginView.tsx|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "./LoginView"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/pages/LoginView.tsx|]

        it "resolves a parent-directory relative import (../)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [[osp|/home/repo/src/utils/math.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "../utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/utils/math.ts|]

        it "resolves current directory root (.) to an index file" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [[osp|/home/repo/src/pages/index.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles "."
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/pages/index.ts|]

        it "resolves parent directory root (..) to an index file" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [[osp|/home/repo/src/index.ts|]]

            let result = runResolveTestFrom importer baseCfg existingFiles ".."
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/index.ts|]

        it "resolves multi-level parent directory relative imports (../../)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/dashboard/User.tsx|]
            let existingFiles = [[osp|/home/repo/src/lib/api.ts|]]

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
            let existingFiles = [[osp|/home/repo/src/pages/utils/math.ts|]]

            let result = runResolveTestFrom importer cfg existingFiles "./utils/math"
            result `shouldBe` absPathUnsafe [osp|/home/repo/src/pages/utils/math.ts|]

    describe "reverseResolveImport" $ do
        let dummyBaseUrl = absPathUnsafe [osp|/home/repo|]
        let baseCfg = TsConfig {baseUrl = dummyBaseUrl, paths = []}

        let runRRTest importerAbsPath cfg existingFiles mIdStr =
                runPureEff
                    . runMockRoFileSystem (mockFiles existingFiles)
                    . runReader cfg
                    $ reverseResolveImport importerAbsPath (ModuleId mIdStr)

        it "converts a parent-directory relative import to an aliased import if a mapping exists" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            let existingFiles = [[osp|/home/repo/src/utils/math.ts|]]

            let result = runRRTest importer cfg existingFiles "../utils/math"
            result `shouldBe` ModuleId "@utils/math"

        it "converts a same-directory relative import to an aliased import if a mapping exists" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@pages/" "") [Wildcard "src/pages/" ""]]}
            let existingFiles = [[osp|/home/repo/src/pages/LoginView.tsx|]]

            let result = runRRTest importer cfg existingFiles "./LoginView"
            result `shouldBe` ModuleId "@pages/LoginView"

        it "converts a relative import to a baseUrl-relative absolute import if no path mapping exists (inside baseUrl)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let existingFiles = [[osp|/home/repo/src/utils/math.ts|]]

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
            let existingFiles = [[osp|/home/repo/src/components/ui/button.tsx|]]

            -- Original import used the broader `@components/` alias
            let result = runRRTest importer cfg existingFiles "@components/ui/button"
            -- It should upgrade to the more specific `@ui/` alias
            result `shouldBe` ModuleId "@ui/button"

        it "leaves an aliased import as-is if it is already the optimal choice" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}
            let existingFiles = [[osp|/home/repo/src/utils/math.ts|]]

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
            let existingFiles = [[osp|/home/repo/src/utils/index.ts|]]

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
            let existingFiles = [[osp|/home/shared/types.ts|]]

            let result = runRRTest importer cfg existingFiles "../../../shared/types"
            result `shouldBe` ModuleId "../../../shared/types"

        it "converts an outside-baseUrl relative import to an aliased import if an explicit mapping exists for it" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@shared/" "") [Wildcard "../shared/" ""]]
                        }
            -- Aliases can map out of the baseUrl via "../"
            let existingFiles = [[osp|/home/shared/utils.ts|]]

            let result = runRRTest importer cfg existingFiles "../../../shared/utils"
            result `shouldBe` ModuleId "@shared/utils"

        it "leaves outside-baseUrl relative imports as-is even if they share folder names with inside-baseUrl paths" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = []}

            -- Target is /home/src/utils.ts (Outside baseUrl, but shares 'src' name)
            let existingFiles = [[osp|/home/src/utils.ts|]]

            -- Should NOT resolve to "src/utils" because it's not the /home/repo/src/utils
            let result = runRRTest importer cfg existingFiles "../../../src/utils"
            result `shouldBe` ModuleId "../../../src/utils"

        it "returns Nothing (preserves target) if the file doesn't exist on disk" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@utils/" "") [Wildcard "src/utils/" ""]]}

            -- The user imports a file that hasn't been created yet, or they made a typo.
            -- `resolve` will fall back to returning the raw fallback absolute path.
            -- `reverseResolve` should gracefully handle this and not crash.
            let existingFiles = []

            let result = runRRTest importer cfg existingFiles "../utils/typo"
            result `shouldBe` ModuleId "@utils/typo"

        it "prioritizes exact path mappings over wildcard mappings in a Next.js environment" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/components/Header.tsx|]
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping (Exact "react") [Exact "node_modules/react"]
                            , -- A common Next.js pattern to map an entire folder
                              mkMapping (Wildcard "@/*" "") [Wildcard "src/*" ""]
                            , -- But specific files might have explicit overrides
                              mkMapping (Exact "@data/users") [Exact "src/data/mock-users"]
                            ]
                        }
            let existingFiles = [[osp|/home/repo/src/data/mock-users.ts|]]

            let result = runRRTest importer cfg existingFiles "../data/mock-users"
            result `shouldBe` ModuleId "@data/users"

        it "resolves a Next.js root alias (@/) correctly" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/app/dashboard/page.tsx|]
            -- Next.js 13+ default alias
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "" ""]]}
            let existingFiles = [[osp|/home/repo/src/lib/utils.ts|]]

            let result = runRRTest importer cfg existingFiles "../../lib/utils"
            result `shouldBe` ModuleId "@/src/lib/utils"

        it "handles 'index.ts' correctly when exact matching a directory alias" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/Home.tsx|]
            let cfg =
                    baseCfg
                        { paths =
                            -- Some codebases map a folder exactly to its index
                            [ mkMapping (Exact "@models") [Exact "src/models/index"]
                            , mkMapping (Wildcard "@models/" "") [Wildcard "src/models/" ""]
                            ]
                        }
            let existingFiles = [[osp|/home/repo/src/models/index.ts|]]

            let result = runRRTest importer cfg existingFiles "../models"
            result `shouldBe` ModuleId "@models"

        it "preserves absolute imports that do not map to the current project (Node built-ins)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/api/route.ts|]
            let cfg = baseCfg {paths = []}
            let existingFiles = []

            -- Imports like "fs", "path", or "crypto"
            let result = runRRTest importer cfg existingFiles "fs"
            result `shouldBe` ModuleId "fs"

        it "preserves complex relative traversals that ultimately resolve inside the baseUrl" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/components/ui/Button.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@hooks/" "") [Wildcard "src/hooks/" ""]]}
            let existingFiles = [[osp|/home/repo/src/hooks/useToggle.ts|]]

            -- A messy relative import: go up, into another folder, back up, then to target
            let result = runRRTest importer cfg existingFiles "../../utils/../hooks/useToggle"

            -- It should figure out exactly where that points and give the clean alias!
            result `shouldBe` ModuleId "@hooks/useToggle"

        it "cleans a deep relative import in Next.js App Router (src/app/(auth)/login/page.tsx)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/app/(auth)/login/page.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let existingFiles = [[osp|/home/repo/src/components/ui/Input.tsx|]]

            -- Dev used a messy relative path to jump out of the grouping folder (auth)
            let result = runRRTest importer cfg existingFiles "../../../components/ui/Input"
            result `shouldBe` ModuleId "@/components/ui/Input"

        it "converts a relative import to a specific feature alias (@feature/*)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/main.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@dashboard/" "") [Wildcard "src/modules/dashboard/" ""]]}
            let existingFiles = [[osp|/home/repo/src/modules/dashboard/components/Chart.tsx|]]

            let result = runRRTest importer cfg existingFiles "./modules/dashboard/components/Chart"
            result `shouldBe` ModuleId "@dashboard/components/Chart"

        it "handles Next.js 'public' folder aliasing for static assets" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/components/Hero.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@public/" "") [Wildcard "public/" ""]]}
            let existingFiles = [[osp|/home/repo/public/vectors/banner.svg|]]

            -- Importing an asset relatively
            let result = runRRTest importer cfg existingFiles "../../public/vectors/banner.svg"
            result `shouldBe` ModuleId "@public/vectors/banner.svg"

        it "prefers a more specific alias over a general root alias (@components/ vs @/)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/pages/index.tsx|]
            let cfg =
                    baseCfg
                        { paths =
                            [ mkMapping (Wildcard "@components/" "") [Wildcard "src/components/" ""]
                            , mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]
                            ]
                        }
            let existingFiles = [[osp|/home/repo/src/components/Button.tsx|]]

            let result = runRRTest importer cfg existingFiles "../components/Button"
            -- Should pick @components/ because it's higher priority in the list
            result `shouldBe` ModuleId "@components/Button"

        it "correctly aliases a sibling file in a flat Vite 'src' structure" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/App.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let existingFiles = [[osp|/home/repo/src/theme.ts|]]

            let result = runRRTest importer cfg existingFiles "./theme"
            result `shouldBe` ModuleId "@/theme"

        it "upgrades a relative import in a monorepo to a cross-package alias (@repo/shared)" $ do
            let importer = absPathUnsafe [osp|/home/repo/apps/web/src/App.tsx|]
            -- baseUrl is at the app level
            let cfg =
                    TsConfig
                        { baseUrl = absPathUnsafe [osp|/home/repo/apps/web|]
                        , paths = [mkMapping (Wildcard "@repo/shared/" "") [Wildcard "../../packages/shared/src/" ""]]
                        }
            let existingFiles = [[osp|/home/repo/packages/shared/src/api.ts|]]

            -- Dev used a messy relative path to reach out of the app into a sibling package
            let result = runRRTest importer cfg existingFiles "../../../packages/shared/src/api"
            result `shouldBe` ModuleId "@repo/shared/api"

        it "handles Next.js App Router 'page to component' imports via root alias" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/app/blog/[slug]/page.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let existingFiles = [[osp|/home/repo/src/components/PostView.tsx|]]

            -- Deeply nested page importing a component
            let result = runRRTest importer cfg existingFiles "../../../components/PostView"
            result `shouldBe` ModuleId "@/components/PostView"

        it "correctly aliases a sibling directory import that uses an index file" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/features/auth/login.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@features/" "") [Wildcard "src/features/" ""]]}
            -- Target is src/features/ui/index.tsx
            let existingFiles = [[osp|/home/repo/src/features/ui/index.tsx|]]

            let result = runRRTest importer cfg existingFiles "../ui"
            result `shouldBe` ModuleId "@features/ui/index"

        it "preserves a relative import to a local JSON configuration file with extension" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/main.ts|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let existingFiles = [[osp|/home/repo/src/config.json|]]

            -- JSON imports must keep their extension
            let result = runRRTest importer cfg existingFiles "./config.json"
            result `shouldBe` ModuleId "@/config.json"

        it "handles Vite's common 'virtual' or prefixed internal modules without breaking them" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/main.tsx|]
            let cfg = baseCfg {paths = []}
            let existingFiles = []

            -- Vite uses virtual modules like 'virtual:pwa-register'
            let result = runRRTest importer cfg existingFiles "virtual:pwa-register"
            result `shouldBe` ModuleId "virtual:pwa-register"

        it "preserves Vite resource queries (e.g., ?raw, ?worker) as they are virtual module references" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/components/Icon.tsx|]
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@assets/" "") [Wildcard "src/assets/" ""]]}
            -- The file exists on disk, but the ?raw query parameter makes the exact disk path probe fail.
            -- This correctly triggers the graceful fallback to preserve the original import string.
            let existingFiles = [[osp|/home/repo/src/assets/logo.svg|]]

            let result = runRRTest importer cfg existingFiles "../../assets/logo.svg?raw"
            result `shouldBe` ModuleId "assets/logo.svg?raw"

        it "preserves Node.js package.json subpath imports (e.g., #internal/utils)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/index.ts|]
            let cfg = baseCfg {paths = []}
            let existingFiles = []

            -- Subpath imports start with '#' and are resolved by Node's export maps, not TS paths.
            let result = runRRTest importer cfg existingFiles "#internal/utils"
            result `shouldBe` ModuleId "#internal/utils"

        it "preserves bare specifiers that look like relative paths due to scoped packages (@org/pkg/.)" $ do
            let importer = absPathUnsafe [osp|/home/repo/src/index.ts|]
            let cfg = baseCfg {paths = []}
            let existingFiles = []

            -- An edge case where a scoped package might have a subpath that tricks naive parsers
            let result = runRRTest importer cfg existingFiles "@company/internal-lib/./utils"
            result `shouldBe` ModuleId "@company/internal-lib/utils"
