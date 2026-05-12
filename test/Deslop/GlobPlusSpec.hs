module Deslop.GlobPlusSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Deslop.GlobPlus
import TestUtils (requireJust)

spec :: Spec
spec = describe "Deslop.GlobPLus" $ do
    describe "matchTarget" $ do
        it "matches exact literal paths and derives TARGET_DIR" $ do
            let target = unsafeCompileTarget "src/app/page"
            fmap (.targetDir) (matchTarget target "src/app/page") `shouldBe` Just "src/app"

        it "returns Nothing when a literal path does not match" $ do
            let target = unsafeCompileTarget "src/app/page"
            matchTarget target "src/app/other" `shouldBe` Nothing

        it "matches wildcards (* and **) and derives the correct directory" $ do
            let target = unsafeCompileTarget "@/features/**/components/*"
            fmap (.targetDir) (matchTarget target "@/features/users/auth/components/Button")
                `shouldBe` Just "@/features/users/auth/components"

        it "* does not match across path separators" $ do
            let target = unsafeCompileTarget "@/features/*/page"
            matchTarget target "@/features/auth/login/page" `shouldBe` Nothing
            matchTarget target "@/features/home/page" `shouldNotBe` Nothing

        it "extracts {{FileName}} (PascalCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/UserSettingsView"

            env.targetDir `shouldBe` "@/features"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserSettings"
            Map.lookup CamelCase env.casings `shouldBe` Just "userSettings"
            Map.lookup KebabCase env.casings `shouldBe` Just "user-settings"
            Map.lookup ConstantCase env.casings `shouldBe` Just "USER_SETTINGS"

        it "rejects a path whose casing does not match the variable token" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            -- lowercase start violates {{FileName}} which requires [A-Z][a-zA-Z0-9]*
            matchTarget target "@/features/userSettingsView" `shouldBe` Nothing

        it "extracts {{fileName}} (camelCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{fileName}}Controller"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/userProfileController"

            Map.lookup CamelCase env.casings `shouldBe` Just "userProfile"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserProfile"
            Map.lookup KebabCase env.casings `shouldBe` Just "user-profile"
            Map.lookup ConstantCase env.casings `shouldBe` Just "USER_PROFILE"

        it "extracts {{file-name}} (kebab-case) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{file-name}}-repository"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/user-settings-repository"

            Map.lookup KebabCase env.casings `shouldBe` Just "user-settings"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserSettings"
            Map.lookup CamelCase env.casings `shouldBe` Just "userSettings"
            Map.lookup ConstantCase env.casings `shouldBe` Just "USER_SETTINGS"

        it "extracts {{FileName}} preceded by a literal prefix (use{{FileName}}ViewModel)" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/auth/useUserAuthViewModel"

            env.targetDir `shouldBe` "@/features/auth"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserAuth"
            Map.lookup KebabCase env.casings `shouldBe` Just "user-auth"

        it "does not match when the literal prefix differs from the pattern" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            matchTarget target "@/features/auth/getUserAuthViewModel" `shouldBe` Nothing

        it "extracts {{FileName}} surrounded by literal prefix and suffix (use{{FileName}}ViewModel.spec)" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel.spec"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/auth/useUserAuthViewModel.spec"

            env.targetDir `shouldBe` "@/features/auth"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserAuth"

        it "derives all casings correctly for a single-word name" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/HomeView"

            Map.lookup PascalCase env.casings `shouldBe` Just "Home"
            Map.lookup CamelCase env.casings `shouldBe` Just "home"
            Map.lookup KebabCase env.casings `shouldBe` Just "home"
            Map.lookup ConstantCase env.casings `shouldBe` Just "HOME"

        it "derives all casings correctly for a three-word compound name" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/admin/UserProfileSettingsContainer"

            env.targetDir `shouldBe` "@/features/admin"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserProfileSettings"
            Map.lookup CamelCase env.casings `shouldBe` Just "userProfileSettings"
            Map.lookup KebabCase env.casings `shouldBe` Just "user-profile-settings"
            Map.lookup ConstantCase env.casings `shouldBe` Just "USER_PROFILE_SETTINGS"

        it "derives TARGET_DIR correctly for deeply nested paths" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/auth/oauth/google/GoogleAuthContainer"

            env.targetDir `shouldBe` "@/features/auth/oauth/google"
            Map.lookup PascalCase env.casings `shouldBe` Just "GoogleAuth"

        it "/**/* matches zero dirs which includes /*" $ do
            let target = unsafeCompileTarget "@/lib/**/*"
            matchTarget target "@/lib/jwt" `shouldSatisfy` isJust
            matchTarget target "@/lib/auth/user" `shouldSatisfy` isJust

        it "** matches everything" $ do
            let target = unsafeCompileTarget "**"
            matchTarget target "a" `shouldSatisfy` isJust
            matchTarget target "lib/a" `shouldSatisfy` isJust
            matchTarget target "dir/" `shouldSatisfy` isJust
            matchTarget target "dir1/dir2/b" `shouldSatisfy` isJust

        it "@/lib/** matches any depth under @/lib/ but not @/lib itself" $ do
            let target = unsafeCompileTarget "@/lib/**"
            matchTarget target "@/lib/jwt" `shouldSatisfy` isJust
            matchTarget target "@/lib/auth/user" `shouldSatisfy` isJust
            matchTarget target "@/lib/a/b/c/d" `shouldSatisfy` isJust
            matchTarget target "@/lib" `shouldBe` Nothing

        it "/**/* does not match the base path or a different prefix" $ do
            let target = unsafeCompileTarget "@/lib/**/*"
            matchTarget target "@/lib" `shouldBe` Nothing
            matchTarget target "@/other/jwt" `shouldBe` Nothing

        it "**/* matches any path at any depth including a single segment" $ do
            let target = unsafeCompileTarget "**/*"
            matchTarget target "jwt" `shouldSatisfy` isJust
            matchTarget target "lib/jwt" `shouldSatisfy` isJust
            matchTarget target "a/b/c" `shouldSatisfy` isJust

        it "multiple ** globstars each independently match zero or more dirs" $ do
            let target = unsafeCompileTarget "@/a/**/b/**/*"
            matchTarget target "@/a/b/c" `shouldSatisfy` isJust
            matchTarget target "@/a/x/b/y/c" `shouldSatisfy` isJust
            matchTarget target "@/a/x/y/b/z/w/c" `shouldSatisfy` isJust

        -- \* single-segment wildcard
        it "* matches any single segment" $ do
            let target = unsafeCompileTarget "@/lib/*"
            matchTarget target "@/lib/jwt" `shouldSatisfy` isJust
            matchTarget target "@/lib/user-auth" `shouldSatisfy` isJust
            matchTarget target "@/lib/UserProfile" `shouldSatisfy` isJust

        it "* does not match two or more segments" $ do
            let target = unsafeCompileTarget "@/lib/*"
            matchTarget target "@/lib/auth/jwt" `shouldBe` Nothing
            matchTarget target "@/lib/a/b/c" `shouldBe` Nothing

        it "* does not match the base path with the trailing segment missing" $ do
            let target = unsafeCompileTarget "@/lib/*"
            matchTarget target "@/lib" `shouldBe` Nothing

        it "* in the middle matches exactly one segment at that position" $ do
            let target = unsafeCompileTarget "@/lib/*/route"
            matchTarget target "@/lib/auth/route" `shouldSatisfy` isJust
            matchTarget target "@/lib/route" `shouldBe` Nothing
            matchTarget target "@/lib/auth/user/route" `shouldBe` Nothing

        it "multiple * each constrain exactly one segment" $ do
            let target = unsafeCompileTarget "@/lib/*/*"
            matchTarget target "@/lib/a/b" `shouldSatisfy` isJust
            matchTarget target "@/lib/a" `shouldBe` Nothing
            matchTarget target "@/lib/a/b/c" `shouldBe` Nothing

        -- \** recursive wildcard
        it "**/segment matches that segment at any depth including zero" $ do
            let target = unsafeCompileTarget "**/index"
            matchTarget target "index" `shouldSatisfy` isJust
            matchTarget target "src/index" `shouldSatisfy` isJust
            matchTarget target "src/app/index" `shouldSatisfy` isJust
            matchTarget target "src/app/deep/index" `shouldSatisfy` isJust
            matchTarget target "other" `shouldBe` Nothing

        it "**/segment does not match paths whose final segment only shares a suffix" $ do
            let target = unsafeCompileTarget "**/components"
            matchTarget target "xcomponents" `shouldBe` Nothing
            matchTarget target "a/xcomponents" `shouldBe` Nothing
            matchTarget target "components/child" `shouldBe` Nothing

        it "@/features/**/{{FileName}} at zero subdirs extracts the variable correctly" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            envZero <-
                requireJust "zero-subdir match failed" $
                    matchTarget target "@/features/HomeContainer"
            Map.lookup PascalCase envZero.casings `shouldBe` Just "Home"
            envOne <-
                requireJust "one-subdir match failed" $
                    matchTarget target "@/features/auth/HomeContainer"
            Map.lookup PascalCase envOne.casings `shouldBe` Just "Home"

        it "* and ** are distinct: * stops at a slash, ** crosses slashes" $ do
            let star = unsafeCompileTarget "@/lib/*"
            let globStar = unsafeCompileTarget "@/lib/**"
            matchTarget star "@/lib/a/b" `shouldBe` Nothing
            matchTarget globStar "@/lib/a/b" `shouldSatisfy` isJust

        it "extracts {{FILE_NAME}} (ConstantCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "src/constants/{{FILE_NAME}}"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "src/constants/MAX_RETRY_COUNT"
            env.targetDir `shouldBe` "src/constants"
            Map.lookup ConstantCase env.casings `shouldBe` Just "MAX_RETRY_COUNT"
            Map.lookup PascalCase env.casings `shouldBe` Just "MaxRetryCount"
            Map.lookup CamelCase env.casings `shouldBe` Just "maxRetryCount"
            Map.lookup KebabCase env.casings `shouldBe` Just "max-retry-count"

        it "rejects a path whose casing does not satisfy {{FILE_NAME}} (requires [A-Z0-9_]+)" $ do
            let target = unsafeCompileTarget "src/constants/{{FILE_NAME}}"
            matchTarget target "src/constants/maxRetryCount" `shouldBe` Nothing
            matchTarget target "src/constants/max-retry-count" `shouldBe` Nothing

        it "treats an all-uppercase captured word as a single token (HTTP -> http)" $ do
            let target = unsafeCompileTarget "@/services/{{FileName}}Client"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/services/HTTPClient"
            -- Original PascalCase capture is preserved by Map.union
            Map.lookup PascalCase env.casings `shouldBe` Just "HTTP"
            -- Derived casings treat "HTTP" as one word
            Map.lookup KebabCase env.casings `shouldBe` Just "http"
            Map.lookup CamelCase env.casings `shouldBe` Just "http"
            Map.lookup ConstantCase env.casings `shouldBe` Just "HTTP"

        it "extracts {{FileName}} with an embedded digit (OAuth2Service)" $ do
            let target = unsafeCompileTarget "@/services/{{FileName}}Service"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/services/OAuth2Service"
            Map.lookup PascalCase env.casings `shouldBe` Just "OAuth2"
            Map.lookup CamelCase env.casings `shouldBe` Just "oAuth2"
            Map.lookup KebabCase env.casings `shouldBe` Just "o-auth2"
            Map.lookup ConstantCase env.casings `shouldBe` Just "O_AUTH2"

        it "extracts {{file-name}} combined with ** at zero subdirs" $ do
            let target = unsafeCompileTarget "@/features/**/{{file-name}}-service"
            envZero <-
                requireJust "zero-subdir failed" $
                    matchTarget target "@/features/user-auth-service"
            envZero.targetDir `shouldBe` "@/features"
            Map.lookup KebabCase envZero.casings `shouldBe` Just "user-auth"
            Map.lookup PascalCase envZero.casings `shouldBe` Just "UserAuth"
            envOne <-
                requireJust "one-subdir failed" $
                    matchTarget target "@/features/auth/user-auth-service"
            envOne.targetDir `shouldBe` "@/features/auth"
            Map.lookup KebabCase envOne.casings `shouldBe` Just "user-auth"

        it "escapes dots in literal path segments of a target pattern" $ do
            let target = unsafeCompileTarget "src/utils.lib/{{FileName}}"
            matchTarget target "src/utils.lib/HomeView" `shouldSatisfy` isJust
            matchTarget target "src/utils_lib/HomeView" `shouldBe` Nothing
            matchTarget target "src/utilsXlib/HomeView" `shouldBe` Nothing

        it "derives empty string as TARGET_DIR for a root-level (single-segment) file" $ do
            let target = unsafeCompileTarget "{{FileName}}"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "HomeView"
            env.targetDir `shouldBe` ""
            Map.lookup PascalCase env.casings `shouldBe` Just "HomeView"

        -- TypeScript cross-casing: KebabCase ↔ PascalCase ↔ CamelCase
        it "derives all casings correctly for a three-word kebab-case name" $ do
            -- Typical TypeScript: file named user-profile-card.tsx → component UserProfileCard
            let target = unsafeCompileTarget "@/components/{{file-name}}"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/user-profile-card"
            Map.lookup KebabCase env.casings `shouldBe` Just "user-profile-card"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserProfileCard"
            Map.lookup CamelCase env.casings `shouldBe` Just "userProfileCard"
            Map.lookup ConstantCase env.casings `shouldBe` Just "USER_PROFILE_CARD"

        it "derives all casings correctly for a three-word camelCase name" $ do
            -- Typical TypeScript: service/hook named userProfileCardService
            let target = unsafeCompileTarget "@/services/{{fileName}}Service"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/services/userProfileCardService"
            Map.lookup CamelCase env.casings `shouldBe` Just "userProfileCard"
            Map.lookup PascalCase env.casings `shouldBe` Just "UserProfileCard"
            Map.lookup KebabCase env.casings `shouldBe` Just "user-profile-card"
            Map.lookup ConstantCase env.casings `shouldBe` Just "USER_PROFILE_CARD"

    describe "matchClause" $ do
        let sampleEnv =
                MatchEnv
                    { targetDir = "@/features/user"
                    , casings =
                        Map.fromList
                            [ (PascalCase, "UserSettings")
                            , (KebabCase, "user-settings")
                            ]
                    }

        -- Full environment with all four casings (as produced by matchTarget + enrichCasings)
        let richEnv =
                MatchEnv
                    { targetDir = "@/features/home"
                    , casings =
                        Map.fromList
                            [ (PascalCase, "HomeProfile")
                            , (CamelCase, "homeProfile")
                            , (KebabCase, "home-profile")
                            , (ConstantCase, "HOME_PROFILE")
                            ]
                    }

        it "interpolates {{TARGET_DIR}} and static strings successfully" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/data/repository"
            matchClause rule sampleEnv "@/features/user/data/repository" `shouldBe` True

        it "interpolates {{file-name}} casings correctly" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/data/{{file-name}}-repository"
            matchClause rule sampleEnv "@/features/user/data/user-settings-repository" `shouldBe` True

        it "rejects paths where the interpolated variables are incorrect" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/data/{{file-name}}-repository"
            -- Mismatched directory
            matchClause rule sampleEnv "@/features/other/data/user-settings-repository" `shouldBe` False
            -- Wrong casing (PascalCase instead of kebab-case)
            matchClause rule sampleEnv "@/features/user/data/UserSettings-repository" `shouldBe` False

        it "handles globs correctly alongside variables" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/*{{FileName}}*"
            matchClause rule sampleEnv "@/features/user/components/buttons/UserSettingsButton" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/components/buttons/OtherButton" `shouldBe` False

        it "interpolates {{FileName}} (PascalCase) into a rule" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClause rule richEnv "@/features/home/HomeProfileView" `shouldBe` True
            matchClause rule richEnv "@/features/home/homeProfileView" `shouldBe` False

        it "interpolates {{fileName}} (camelCase) into a rule" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{fileName}}Service"
            matchClause rule richEnv "@/features/home/homeProfileService" `shouldBe` True
            matchClause rule richEnv "@/features/home/HomeProfileService" `shouldBe` False

        it "interpolates {{FILE_NAME}} (CONSTANT_CASE) into a rule" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FILE_NAME}}_config"
            matchClause rule richEnv "@/features/home/HOME_PROFILE_config" `shouldBe` True
            matchClause rule richEnv "@/features/home/home-profile_config" `shouldBe` False

        it "interpolates a literal prefix alongside {{FileName}} (use{{FileName}}ViewModel)" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            matchClause rule richEnv "@/features/home/useHomeProfileViewModel" `shouldBe` True
            matchClause rule richEnv "@/features/home/HomeProfileViewModel" `shouldBe` False

        it "matches a .spec existence pattern" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            matchClause rule richEnv "@/features/home/useHomeProfileViewModel.spec" `shouldBe` True
            matchClause rule richEnv "@/features/home/useHomeProfileViewModel.test" `shouldBe` False

        it "matches a .stories existence pattern" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View.stories"
            matchClause rule richEnv "@/features/home/HomeProfileView.stories" `shouldBe` True
            matchClause rule richEnv "@/features/home/HomeProfileView.spec" `shouldBe` False

        it "falls back to .* when a casing key is absent from the environment" $ do
            let sparseEnv = MatchEnv {targetDir = "@/features/x", casings = Map.empty}
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            -- Missing casing → .* matches any value in that slot
            matchClause rule sparseEnv "@/features/x/AnythingView" `shouldBe` True
            matchClause rule sparseEnv "@/features/x/SomethingElseView" `shouldBe` True
            -- TARGET_DIR is still exact
            matchClause rule sparseEnv "@/features/other/AnythingView" `shouldBe` False

        -- \* in rules
        it "* in a rule matches exactly one segment" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/*"
            matchClause rule sampleEnv "@/features/user/anything" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/a/b" `shouldBe` False
            matchClause rule sampleEnv "@/features/other/anything" `shouldBe` False

        it "* in the middle of a rule does not cross a path separator" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/*/index"
            matchClause rule sampleEnv "@/features/user/components/index" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/a/b/index" `shouldBe` False
            matchClause rule sampleEnv "@/features/user/index" `shouldBe` False

        -- \** in rules
        it "{{TARGET_DIR}}/**/* matches zero subdirs (rule regression)" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/*"
            matchClause rule sampleEnv "@/features/user/Button" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/components/Button" `shouldBe` True
            matchClause rule sampleEnv "@/features/other/Button" `shouldBe` False

        it "{{TARGET_DIR}}/** matches any path at any depth below TARGET_DIR" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**"
            matchClause rule sampleEnv "@/features/user/anything" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/a/b/c" `shouldBe` True
            matchClause rule sampleEnv "@/features/other/anything" `shouldBe` False

        it "{{TARGET_DIR}}/**/*.spec matches .spec files at any depth including zero subdirs" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/*.spec"
            matchClause rule sampleEnv "@/features/user/Button.spec" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/components/Button.spec" `shouldBe` True
            matchClause rule sampleEnv "@/features/user/Button.test" `shouldBe` False
            matchClause rule sampleEnv "@/features/other/Button.spec" `shouldBe` False

        it "{{TARGET_DIR}}/**/{{FileName}}.spec matches at any depth including zero subdirs" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/{{FileName}}.spec"
            matchClause rule richEnv "@/features/home/HomeProfile.spec" `shouldBe` True
            matchClause rule richEnv "@/features/home/auth/HomeProfile.spec" `shouldBe` True
            matchClause rule richEnv "@/features/home/HomeProfile.test" `shouldBe` False
            matchClause rule richEnv "@/features/other/HomeProfile.spec" `shouldBe` False

        it "escapes regex metacharacters in TARGET_DIR (dot must not match arbitrary chars)" $ do
            let env =
                    MatchEnv
                        { targetDir = "src/v1.0/features"
                        , casings = Map.fromList [(PascalCase, "Home")]
                        }
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClause rule env "src/v1.0/features/HomeView" `shouldBe` True
            matchClause rule env "src/v1X0/features/HomeView" `shouldBe` False

    describe "moduleFromGlob" $ do
        let env =
                MatchEnv
                    { targetDir = "@/features/auth"
                    , casings =
                        Map.fromList
                            [ (PascalCase, "UserAuth")
                            , (CamelCase, "userAuth")
                            , (KebabCase, "user-auth")
                            , (ConstantCase, "USER_AUTH")
                            ]
                    }

        it "expands TARGET_DIR and FileName into a concrete spec path" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/useUserAuthViewModel.spec"

        it "expands TARGET_DIR and FileName into a concrete stories path" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View.stories"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/UserAuthView.stories"

        it "expands TARGET_DIR and file-name into a kebab-case repository path" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{file-name}}-repository"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/user-auth-repository"

        it "expands TARGET_DIR alone" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/index"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/index"

        it "expands a purely literal pattern unchanged" $ do
            let pat = unsafeCompileClause "@/shared/constants"
            moduleFromGlob env pat `shouldBe` Just "@/shared/constants"

        it "returns Nothing when the pattern contains *" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/*.spec"
            moduleFromGlob env pat `shouldBe` Nothing

        it "returns Nothing when the pattern contains **" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/**/*.spec"
            moduleFromGlob env pat `shouldBe` Nothing

        it "returns Nothing for a pattern that is only a glob star" $ do
            let pat = unsafeCompileClause "**"
            moduleFromGlob env pat `shouldBe` Nothing

        it "returns Just with an empty segment when a casing key is absent from the env" $ do
            -- fromMaybe "" means missing keys silently expand to empty string, not Nothing
            let sparseEnv = MatchEnv {targetDir = "@/features/x", casings = Map.empty}
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            moduleFromGlob sparseEnv pat `shouldBe` Just "@/features/x/View"

    describe "renderClausePattern" $ do
        let env =
                MatchEnv
                    { targetDir = "@/features/auth"
                    , casings =
                        Map.fromList
                            [ (PascalCase, "UserAuth")
                            , (CamelCase, "userAuth")
                            , (KebabCase, "user-auth")
                            , (ConstantCase, "USER_AUTH")
                            ]
                    }

        it "substitutes TARGET_DIR and FileName into a concrete path" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}StateEvent"
            renderClausePattern env pat `shouldBe` "@/features/auth/UserAuthStateEvent"

        it "substitutes TARGET_DIR and file-name into a kebab-case path" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{file-name}}-repository"
            renderClausePattern env pat `shouldBe` "@/features/auth/user-auth-repository"

        it "substitutes TARGET_DIR and use{{FileName}}ViewModel pattern" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            renderClausePattern env pat `shouldBe` "@/features/auth/useUserAuthViewModel"

        it "keeps * wildcards literally" $ do
            let pat = unsafeCompileClause "@/features/**/*Container"
            renderClausePattern env pat `shouldBe` "@/features/**/*Container"

        it "keeps ** wildcards literally" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/**/*.spec"
            renderClausePattern env pat `shouldBe` "@/features/auth/**/*.spec"

        it "renders a purely literal pattern unchanged" $ do
            let pat = unsafeCompileClause "@/shared/constants"
            renderClausePattern env pat `shouldBe` "@/shared/constants"

        it "renders TARGET_DIR alone" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/index"
            renderClausePattern env pat `shouldBe` "@/features/auth/index"

        it "falls back to the variable placeholder when a casing key is absent" $ do
            let sparseEnv = MatchEnv {targetDir = "@/features/x", casings = Map.empty}
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            renderClausePattern sparseEnv pat `shouldBe` "@/features/x/{{FileName}}View"

    describe "End-to-End Scenarios" $ do
        it "validates the Page Architecture ViewModel rule end-to-end" $ do
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cRule = unsafeCompileClause "{{TARGET_DIR}}/data/{{file-name}}-repository"
            let targetPath = "@/features/auth/useUserAuthViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchClause cRule env "@/features/auth/data/user-auth-repository" `shouldBe` True
            matchClause cRule env "@/features/auth/data/global-repository" `shouldBe` False
            matchClause cRule env "@/features/other/data/user-auth-repository" `shouldBe` False

        it "validates the Container wires View and ViewModel (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            let cStateEvent = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}StateEvent"
            let cViewModel = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            let cView = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            let targetPath = "@/features/checkout/PaymentContainer"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchClause cStateEvent env "@/features/checkout/PaymentStateEvent" `shouldBe` True
            matchClause cViewModel env "@/features/checkout/usePaymentViewModel" `shouldBe` True
            matchClause cView env "@/features/checkout/PaymentView" `shouldBe` True
            -- Wrong feature dir
            matchClause cStateEvent env "@/features/home/PaymentStateEvent" `shouldBe` False
            -- Wrong component name
            matchClause cViewModel env "@/features/checkout/useCheckoutViewModel" `shouldBe` False

        it "validates the ViewModel test existence rule (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cSpec = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            let targetPath = "@/features/auth/useUserAuthViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchClause cSpec env "@/features/auth/useUserAuthViewModel.spec" `shouldBe` True
            matchClause cSpec env "@/features/auth/useUserAuthViewModel.test" `shouldBe` False
            matchClause cSpec env "@/features/other/useUserAuthViewModel.spec" `shouldBe` False

        it "validates the View Storybook existence rule (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}View"
            let cStories = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View.stories"
            let targetPath = "@/features/profile/UserProfileView"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchClause cStories env "@/features/profile/UserProfileView.stories" `shouldBe` True
            matchClause cStories env "@/features/profile/UserProfileView.storybook" `shouldBe` False
            matchClause cStories env "@/features/profile/UserProfileView.spec" `shouldBe` False

        it "validates the ViewModel forbids-import rule (page-architecture)" $ do
            -- ViewModel must NOT import its own View; matchClause True = forbids path detected
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cforbids = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            let targetPath = "@/features/home/useHomeViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchClause cforbids env "@/features/home/HomeView" `shouldBe` True
            -- Other paths in the same dir are not caught by this forbids rule
            matchClause cforbids env "@/features/home/HomeContainer" `shouldBe` False
            matchClause cforbids env "@/features/other/HomeView" `shouldBe` False

        it "validates a ConstantCase naming convention end-to-end" $ do
            -- Cross-casing from ConstantCase is lossy (see matchTarget tests above),
            -- so same-casing enforcement ({{FILE_NAME}} -> {{FILE_NAME}}) is reliable.
            let cTarget = unsafeCompileTarget "src/constants/{{FILE_NAME}}"
            let cRule = unsafeCompileClause "src/types/{{FILE_NAME}}_types"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget cTarget "src/constants/MAX_RETRY_COUNT"
            -- same casing is preserved exactly, so same-style rules match correctly
            matchClause cRule env "src/types/MAX_RETRY_COUNT_types" `shouldBe` True
            -- wrong constant name
            matchClause cRule env "src/types/MIN_RETRY_COUNT_types" `shouldBe` False
            -- wrong directory
            matchClause cRule env "src/constants/MAX_RETRY_COUNT_types" `shouldBe` False

    describe "TypeScript web codebase patterns" $ do
        -- PascalCase → KebabCase: the canonical React pattern
        it "PascalCase component enforces kebab-case CSS module" $ do
            let cTarget = unsafeCompileTarget "@/components/{{FileName}}"
            let cCssModule = unsafeCompileClause "{{TARGET_DIR}}/{{file-name}}.module.css"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget cTarget "@/components/UserProfileCard"
            matchClause cCssModule env "@/components/user-profile-card.module.css" `shouldBe` True
            -- PascalCase CSS module name is wrong
            matchClause cCssModule env "@/components/UserProfileCard.module.css" `shouldBe` False
            -- Partial name mismatch
            matchClause cCssModule env "@/components/user-profile.module.css" `shouldBe` False

        it "PascalCase component enforces PascalCase stories and spec" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}"
            let cStories = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}.stories"
            let cSpec = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}.spec"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget cTarget "@/features/auth/LoginForm"
            matchClause cStories env "@/features/auth/LoginForm.stories" `shouldBe` True
            matchClause cSpec env "@/features/auth/LoginForm.spec" `shouldBe` True
            -- Kebab-case versions of stories/spec are wrong
            matchClause cStories env "@/features/auth/login-form.stories" `shouldBe` False
            matchClause cSpec env "@/features/auth/login-form.spec" `shouldBe` False

        -- KebabCase → PascalCase: the reverse cross-casing direction
        it "kebab-case file target enforces PascalCase component and camelCase hook rules" $ do
            let cTarget = unsafeCompileTarget "@/components/{{file-name}}"
            let cComponent = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}"
            let cHook = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget cTarget "@/components/login-form"
            matchClause cComponent env "@/components/LoginForm" `shouldBe` True
            matchClause cHook env "@/components/useLoginForm" `shouldBe` True
            -- Kebab casing is wrong in a PascalCase rule slot
            matchClause cComponent env "@/components/login-form" `shouldBe` False
            -- Capital "Use" is wrong (hook prefix is camelCase)
            matchClause cHook env "@/components/UseLoginForm" `shouldBe` False

        -- CamelCase → PascalCase + KebabCase: TypeScript service/interface convention
        it "camelCase service target enforces PascalCase interface and kebab-case spec" $ do
            let cTarget = unsafeCompileTarget "@/services/{{fileName}}Service"
            let cInterface = unsafeCompileClause "{{TARGET_DIR}}/I{{FileName}}Service"
            let cSpec = unsafeCompileClause "{{TARGET_DIR}}/{{file-name}}-service.spec"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget cTarget "@/services/userProfileService"
            matchClause cInterface env "@/services/IUserProfileService" `shouldBe` True
            matchClause cSpec env "@/services/user-profile-service.spec" `shouldBe` True
            -- Wrong casing for interface (lowercase 'i' prefix or wrong name form)
            matchClause cInterface env "@/services/userProfileService" `shouldBe` False
            -- PascalCase spec file name is wrong
            matchClause cSpec env "@/services/UserProfileService.spec" `shouldBe` False

-- Helpers

unsafeCompileTarget :: Text -> CompiledTargetPattern
unsafeCompileTarget t = case parseTargetPattern t of
    Right ast -> compileTargetPattern ast
    Left err -> error $ "Failed to parse target pattern: " <> show err

unsafeCompileClause :: Text -> CompiledClausePattern
unsafeCompileClause t = case parseClausePattern t of
    Right ast -> compileClausePattern ast
    Left err -> error $ "Failed to parse rule pattern: " <> show err
