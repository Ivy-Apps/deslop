module Deslop.GlobPlusSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Deslop.GlobPlus
import TestUtils (requireJust)

spec :: Spec
spec = describe "Deslop.GlobPlus" $ do
    describe "Deslop.GlobPlus.matchTarget" $ do
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

    describe "Deslop.GlobPlus.matchRule" $ do
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
            let rule = unsafeCompileRule "{{TARGET_DIR}}/data/repository"
            matchRule rule sampleEnv "@/features/user/data/repository" `shouldBe` True

        it "interpolates {{file-name}} casings correctly" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/data/{{file-name}}-repository"
            matchRule rule sampleEnv "@/features/user/data/user-settings-repository" `shouldBe` True

        it "rejects paths where the interpolated variables are incorrect" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/data/{{file-name}}-repository"
            -- Mismatched directory
            matchRule rule sampleEnv "@/features/other/data/user-settings-repository" `shouldBe` False
            -- Wrong casing (PascalCase instead of kebab-case)
            matchRule rule sampleEnv "@/features/user/data/UserSettings-repository" `shouldBe` False

        it "handles globs correctly alongside variables" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/**/*{{FileName}}*"
            matchRule rule sampleEnv "@/features/user/components/buttons/UserSettingsButton" `shouldBe` True
            matchRule rule sampleEnv "@/features/user/components/buttons/OtherButton" `shouldBe` False

        it "interpolates {{FileName}} (PascalCase) into a rule" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View"
            matchRule rule richEnv "@/features/home/HomeProfileView" `shouldBe` True
            matchRule rule richEnv "@/features/home/homeProfileView" `shouldBe` False

        it "interpolates {{fileName}} (camelCase) into a rule" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/{{fileName}}Service"
            matchRule rule richEnv "@/features/home/homeProfileService" `shouldBe` True
            matchRule rule richEnv "@/features/home/HomeProfileService" `shouldBe` False

        it "interpolates {{FILE_NAME}} (CONSTANT_CASE) into a rule" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/{{FILE_NAME}}_config"
            matchRule rule richEnv "@/features/home/HOME_PROFILE_config" `shouldBe` True
            matchRule rule richEnv "@/features/home/home-profile_config" `shouldBe` False

        it "interpolates a literal prefix alongside {{FileName}} (use{{FileName}}ViewModel)" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            matchRule rule richEnv "@/features/home/useHomeProfileViewModel" `shouldBe` True
            matchRule rule richEnv "@/features/home/HomeProfileViewModel" `shouldBe` False

        it "matches a .spec existence pattern" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            matchRule rule richEnv "@/features/home/useHomeProfileViewModel.spec" `shouldBe` True
            matchRule rule richEnv "@/features/home/useHomeProfileViewModel.test" `shouldBe` False

        it "matches a .stories existence pattern" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View.stories"
            matchRule rule richEnv "@/features/home/HomeProfileView.stories" `shouldBe` True
            matchRule rule richEnv "@/features/home/HomeProfileView.spec" `shouldBe` False

        it "falls back to .* when a casing key is absent from the environment" $ do
            let sparseEnv = MatchEnv {targetDir = "@/features/x", casings = Map.empty}
            let rule = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View"
            -- Missing casing → .* matches any value in that slot
            matchRule rule sparseEnv "@/features/x/AnythingView" `shouldBe` True
            matchRule rule sparseEnv "@/features/x/SomethingElseView" `shouldBe` True
            -- TARGET_DIR is still exact
            matchRule rule sparseEnv "@/features/other/AnythingView" `shouldBe` False

    describe "Deslop.GlobPlus.moduleFromGlob" $ do
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
            let pat = unsafeCompileRule "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/useUserAuthViewModel.spec"

        it "expands TARGET_DIR and FileName into a concrete stories path" $ do
            let pat = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View.stories"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/UserAuthView.stories"

        it "expands TARGET_DIR and file-name into a kebab-case repository path" $ do
            let pat = unsafeCompileRule "{{TARGET_DIR}}/{{file-name}}-repository"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/user-auth-repository"

        it "expands TARGET_DIR alone" $ do
            let pat = unsafeCompileRule "{{TARGET_DIR}}/index"
            moduleFromGlob env pat `shouldBe` Just "@/features/auth/index"

        it "expands a purely literal pattern unchanged" $ do
            let pat = unsafeCompileRule "@/shared/constants"
            moduleFromGlob env pat `shouldBe` Just "@/shared/constants"

        it "returns Nothing when the pattern contains *" $ do
            let pat = unsafeCompileRule "{{TARGET_DIR}}/*.spec"
            moduleFromGlob env pat `shouldBe` Nothing

        it "returns Nothing when the pattern contains **" $ do
            let pat = unsafeCompileRule "{{TARGET_DIR}}/**/*.spec"
            moduleFromGlob env pat `shouldBe` Nothing

        it "returns Nothing for a pattern that is only a glob star" $ do
            let pat = unsafeCompileRule "**"
            moduleFromGlob env pat `shouldBe` Nothing

    describe "End-to-End Scenarios" $ do
        it "validates the Page Architecture ViewModel rule end-to-end" $ do
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cRule = unsafeCompileRule "{{TARGET_DIR}}/data/{{file-name}}-repository"
            let targetPath = "@/features/auth/useUserAuthViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchRule cRule env "@/features/auth/data/user-auth-repository" `shouldBe` True
            matchRule cRule env "@/features/auth/data/global-repository" `shouldBe` False
            matchRule cRule env "@/features/other/data/user-auth-repository" `shouldBe` False

        it "validates the Container wires View and ViewModel (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            let cStateEvent = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}StateEvent"
            let cViewModel = unsafeCompileRule "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            let cView = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View"
            let targetPath = "@/features/checkout/PaymentContainer"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchRule cStateEvent env "@/features/checkout/PaymentStateEvent" `shouldBe` True
            matchRule cViewModel env "@/features/checkout/usePaymentViewModel" `shouldBe` True
            matchRule cView env "@/features/checkout/PaymentView" `shouldBe` True
            -- Wrong feature dir
            matchRule cStateEvent env "@/features/home/PaymentStateEvent" `shouldBe` False
            -- Wrong component name
            matchRule cViewModel env "@/features/checkout/useCheckoutViewModel" `shouldBe` False

        it "validates the ViewModel test existence rule (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cSpec = unsafeCompileRule "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            let targetPath = "@/features/auth/useUserAuthViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchRule cSpec env "@/features/auth/useUserAuthViewModel.spec" `shouldBe` True
            matchRule cSpec env "@/features/auth/useUserAuthViewModel.test" `shouldBe` False
            matchRule cSpec env "@/features/other/useUserAuthViewModel.spec" `shouldBe` False

        it "validates the View Storybook existence rule (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}View"
            let cStories = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View.stories"
            let targetPath = "@/features/profile/UserProfileView"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchRule cStories env "@/features/profile/UserProfileView.stories" `shouldBe` True
            matchRule cStories env "@/features/profile/UserProfileView.storybook" `shouldBe` False
            matchRule cStories env "@/features/profile/UserProfileView.spec" `shouldBe` False

        it "validates the ViewModel forbidden-import rule (page-architecture)" $ do
            -- ViewModel must NOT import its own View; matchRule True = forbidden path detected
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cForbidden = unsafeCompileRule "{{TARGET_DIR}}/{{FileName}}View"
            let targetPath = "@/features/home/useHomeViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget cTarget targetPath

            matchRule cForbidden env "@/features/home/HomeView" `shouldBe` True
            -- Other paths in the same dir are not caught by this forbidden rule
            matchRule cForbidden env "@/features/home/HomeContainer" `shouldBe` False
            matchRule cForbidden env "@/features/other/HomeView" `shouldBe` False

-- Helpers

unsafeCompileTarget :: Text -> CompiledTargetPattern
unsafeCompileTarget t = case parseTargetPattern t of
    Right ast -> compileTargetPattern ast
    Left err -> error $ "Failed to parse target pattern: " <> show err

unsafeCompileRule :: Text -> CompiledRulePattern
unsafeCompileRule t = case parseRulePattern t of
    Right ast -> compileRulePattern ast
    Left err -> error $ "Failed to parse rule pattern: " <> show err
