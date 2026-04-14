{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolverSpec (spec) where

import Doubles.FileSystem (MockRoFileSystem (..), defaultMockRoFileSystem, runMockRoFileSystem)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (absPathUnsafe)
import System.OsPath (osp)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import TypeScript.ModuleResolver (Match (..), ModuleId (..), match, resolve, reverseResolve)

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

    describe "encode (Reverse Path Resolution)" $ do
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
            result `shouldBe` ModuleId "src/lib/util"

        it "resolves relative to baseUrl when mappings exist but do not match" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/test/util.ts|]
            result `shouldBe` ModuleId "test/util"

        it "applies an Exact path mapping" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Exact "jquery") [Exact "node_modules/jquery/dist/jquery"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/node_modules/jquery/dist/jquery.js|]
            result `shouldBe` ModuleId "jquery"

        it "applies a Suffix Wildcard path mapping" $ do
            let cfg = baseCfg {paths = [mkMapping (Wildcard "@/" "") [Wildcard "src/" ""]]}
            let result = runEncodeTest cfg [osp|/home/repo/src/lib/util.tsx|]
            result `shouldBe` ModuleId "@/lib/util"

        it "applies an Infix Wildcard path mapping" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@dto/" "-dto") [Wildcard "src/types/" "-dto"]]
                        }
            let resMatch = runEncodeTest cfg [osp|/home/repo/src/types/user/account-dto.ts|]
            resMatch `shouldBe` ModuleId "@dto/user/account-dto"
            let resNotFound = runEncodeTest cfg [osp|/home/repo/src/types/user/account.ts|]
            resNotFound `shouldBe` ModuleId "src/types/user/account"

        it "handles prefix wildcards (*-spec)" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@tests/" "-spec") [Wildcard "src/tests/" "-spec"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/src/tests/auth-spec.ts|]
            result `shouldBe` ModuleId "@tests/auth-spec"

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
            result `shouldBe` ModuleId "@utils/math"

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
            result `shouldBe` ModuleId "@utils/math"

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
            result `shouldBe` ModuleId "@libs/logger"

        it "handles Wildcard keys mapped to Exact values" $ do
            let cfg =
                    baseCfg
                        { paths = [mkMapping (Wildcard "@core/" "") [Exact "src/core"]]
                        }
            let result = runEncodeTest cfg [osp|/home/repo/src/core.ts|]
            -- Candidate "src/core" matches Exact "src/core" -> ExactMatch
            -- Applying ExactMatch to Wildcard "@core/" "" -> "@core/" <> "" <> "" -> "@core/"
            result `shouldBe` ModuleId "@core/"

    describe "resolve (Forward Path Resolution)" $ do
        let dummyBaseUrl = absPathUnsafe [osp|/home/repo|]
        let baseCfg = TsConfig {baseUrl = dummyBaseUrl, paths = []}

        let mkMapping k vs = PathMapping (KeyPattern k) (ValuePattern <$> fromList vs)

        -- Helper to run the resolve function with a simulated file system
        let runResolveTest cfg existingFiles mId =
                let mockFs =
                        defaultMockRoFileSystem
                            { mockFileExistsAbs = \p -> pure $ p `elem` existingFiles
                            }
                 in runPureEff
                        . runMockRoFileSystem mockFs
                        . runReader cfg
                        $ resolve (ModuleId mId)

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
