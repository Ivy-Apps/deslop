{-# LANGUAGE QuasiQuotes #-}

module TypeScript.ModuleResolverSpec (spec) where

import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (absPathUnsafe)
import System.OsPath (osp)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.Config (KeyPattern (..), PathMapping (..), Pattern (..), TsConfig (..), ValuePattern (..))
import TypeScript.ModuleResolver (Match (..), ModuleId (..), encode, match)

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
                            $ encode path

            it "resolves relative to baseUrl when there are no path mappings" $ do
                let result = runEncodeTest baseCfg [osp|/home/repo/src/lib/util.tsx|]
                result `shouldBe` ModuleId "src/lib/util"

            it "resolves relative to baseUrl when mappings exist but do not match" $ do
                let cfg = baseCfg {paths = [mkMapping (Wildcard "@/*" "") [Wildcard "src/*" ""]]}
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
                let cfg = baseCfg {paths = [mkMapping (Wildcard "@/*" "") [Wildcard "src/*" ""]]}
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
                                    (Wildcard "@utils/*" "")
                                    [ Wildcard "src/utils/*" ""
                                    , Wildcard "shared/utils/*" ""
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
                                , mkMapping (Wildcard "@utils/*" "") [Wildcard "src/utils/*" ""]
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
                                [ mkMapping (Exact "invalid-exact") [Wildcard "src/libs/*" ""]
                                , mkMapping (Wildcard "@libs/*" "") [Wildcard "src/libs/*" ""]
                                ]
                            }
                -- It should hit the first mapping, realize it can't apply a capture to an Exact key,
                -- safely fall through, and successfully match the second mapping.
                let result = runEncodeTest cfg [osp|/home/repo/src/libs/logger.ts|]
                result `shouldBe` ModuleId "@libs/logger"

            it "handles Wildcard keys mapped to Exact values" $ do
                let cfg =
                        baseCfg
                            { paths = [mkMapping (Wildcard "@core/*" "") [Exact "src/core"]]
                            }
                let result = runEncodeTest cfg [osp|/home/repo/src/core.ts|]
                -- Candidate "src/core" matches Exact "src/core" -> ExactMatch
                -- Applying ExactMatch to Wildcard "@core/" "" -> "@core/" <> "" <> "" -> "@core/"
                result `shouldBe` ModuleId "@core/"
