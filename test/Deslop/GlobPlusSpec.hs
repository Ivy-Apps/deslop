module Deslop.GlobPlusSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog (Gen, assert, failure, forAll, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec

import Deslop.GlobPlus
import Deslop.GlobPlus.Compiler
import Deslop.GlobPlusOracle qualified as Oracle
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import TestUtils (prop, requireJust)

spec :: Spec
spec = describe "Deslop.GlobPLus" $ do
    describe "matchTargetAt" $ do
        it "matches exact literal paths and derives TARGET_DIR" $ do
            let target = unsafeCompileTarget "src/app/page"
            fmap (.targetDir) (matchTargetAt target "src/app/page") `shouldBe` Just "src/app"

        it "returns Nothing when a literal path does not match" $ do
            let target = unsafeCompileTarget "src/app/page"
            matchTargetAt target "src/app/other" `shouldBe` Nothing

        it "matches wildcards (* and **) and derives the correct directory" $ do
            let target = unsafeCompileTarget "@/features/**/components/*"
            fmap (.targetDir) (matchTargetAt target "@/features/users/auth/components/Button")
                `shouldBe` Just "@/features/users/auth/components"

        it "* does not match across path separators" $ do
            let target = unsafeCompileTarget "@/features/*/page"
            matchTargetAt target "@/features/auth/login/page" `shouldBe` Nothing
            matchTargetAt target "@/features/home/page" `shouldNotBe` Nothing

        it "extracts {{FileName}} (PascalCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/UserSettingsView"

            env.targetDir `shouldBe` "@/features"
            casingOf PascalCase env `shouldBe` Just "UserSettings"
            casingOf CamelCase env `shouldBe` Just "userSettings"
            casingOf KebabCase env `shouldBe` Just "user-settings"
            casingOf ConstantCase env `shouldBe` Just "USER_SETTINGS"

        it "rejects a path whose casing does not match the variable token" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            -- lowercase start violates {{FileName}} which requires [A-Z][a-zA-Z0-9]*
            matchTargetAt target "@/features/userSettingsView" `shouldBe` Nothing

        it "extracts {{fileName}} (camelCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{fileName}}Controller"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/userProfileController"

            casingOf CamelCase env `shouldBe` Just "userProfile"
            casingOf PascalCase env `shouldBe` Just "UserProfile"
            casingOf KebabCase env `shouldBe` Just "user-profile"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE"

        it "extracts {{file-name}} (kebab-case) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{file-name}}-repository"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/user-settings-repository"

            casingOf KebabCase env `shouldBe` Just "user-settings"
            casingOf PascalCase env `shouldBe` Just "UserSettings"
            casingOf CamelCase env `shouldBe` Just "userSettings"
            casingOf ConstantCase env `shouldBe` Just "USER_SETTINGS"

        it "extracts {{FileName}} preceded by a literal prefix (use{{FileName}}ViewModel)" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/auth/useUserAuthViewModel"

            env.targetDir `shouldBe` "@/features/auth"
            casingOf PascalCase env `shouldBe` Just "UserAuth"
            casingOf KebabCase env `shouldBe` Just "user-auth"

        it "does not match when the literal prefix differs from the pattern" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            matchTargetAt target "@/features/auth/getUserAuthViewModel" `shouldBe` Nothing

        it "extracts {{FileName}} surrounded by literal prefix and suffix (use{{FileName}}ViewModel.spec)" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel.spec"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/auth/useUserAuthViewModel.spec"

            env.targetDir `shouldBe` "@/features/auth"
            casingOf PascalCase env `shouldBe` Just "UserAuth"

        it "derives all casings correctly for a single-word name" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/HomeView"

            casingOf PascalCase env `shouldBe` Just "Home"
            casingOf CamelCase env `shouldBe` Just "home"
            casingOf KebabCase env `shouldBe` Just "home"
            casingOf ConstantCase env `shouldBe` Just "HOME"

        it "derives all casings correctly for a three-word compound name" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/admin/UserProfileSettingsContainer"

            env.targetDir `shouldBe` "@/features/admin"
            casingOf PascalCase env `shouldBe` Just "UserProfileSettings"
            casingOf CamelCase env `shouldBe` Just "userProfileSettings"
            casingOf KebabCase env `shouldBe` Just "user-profile-settings"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE_SETTINGS"

        it "derives TARGET_DIR correctly for deeply nested paths" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt target "@/features/auth/oauth/google/GoogleAuthContainer"

            env.targetDir `shouldBe` "@/features/auth/oauth/google"
            casingOf PascalCase env `shouldBe` Just "GoogleAuth"

        it "/**/* matches zero dirs which includes /*" $ do
            let target = unsafeCompileTarget "@/lib/**/*"
            matchTargetAt target "@/lib/jwt" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/auth/user" `shouldSatisfy` isJust

        it "** matches everything" $ do
            let target = unsafeCompileTarget "**"
            matchTargetAt target "a" `shouldSatisfy` isJust
            matchTargetAt target "lib/a" `shouldSatisfy` isJust
            matchTargetAt target "dir/" `shouldSatisfy` isJust
            matchTargetAt target "dir1/dir2/b" `shouldSatisfy` isJust

        it "@/lib/** matches any depth under @/lib, and @/lib itself" $ do
            let target = unsafeCompileTarget "@/lib/**"
            matchTargetAt target "@/lib/jwt" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/auth/user" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/a/b/c/d" `shouldSatisfy` isJust
            -- ** stands for zero or many segments, and zero is one of them.
            matchTargetAt target "@/lib" `shouldSatisfy` isJust
            matchTargetAt target "@/libs" `shouldBe` Nothing

        it "/**/* does not match the base path or a different prefix" $ do
            let target = unsafeCompileTarget "@/lib/**/*"
            matchTargetAt target "@/lib" `shouldBe` Nothing
            matchTargetAt target "@/other/jwt" `shouldBe` Nothing

        it "**/* matches any path at any depth including a single segment" $ do
            let target = unsafeCompileTarget "**/*"
            matchTargetAt target "jwt" `shouldSatisfy` isJust
            matchTargetAt target "lib/jwt" `shouldSatisfy` isJust
            matchTargetAt target "a/b/c" `shouldSatisfy` isJust

        it "multiple ** globstars each independently match zero or more dirs" $ do
            let target = unsafeCompileTarget "@/a/**/b/**/*"
            matchTargetAt target "@/a/b/c" `shouldSatisfy` isJust
            matchTargetAt target "@/a/x/b/y/c" `shouldSatisfy` isJust
            matchTargetAt target "@/a/x/y/b/z/w/c" `shouldSatisfy` isJust

        -- \* single-segment wildcard
        it "* matches any single segment" $ do
            let target = unsafeCompileTarget "@/lib/*"
            matchTargetAt target "@/lib/jwt" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/user-auth" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/UserProfile" `shouldSatisfy` isJust

        it "* does not match two or more segments" $ do
            let target = unsafeCompileTarget "@/lib/*"
            matchTargetAt target "@/lib/auth/jwt" `shouldBe` Nothing
            matchTargetAt target "@/lib/a/b/c" `shouldBe` Nothing

        it "* does not match the base path with the trailing segment missing" $ do
            let target = unsafeCompileTarget "@/lib/*"
            matchTargetAt target "@/lib" `shouldBe` Nothing

        it "* in the middle matches exactly one segment at that position" $ do
            let target = unsafeCompileTarget "@/lib/*/route"
            matchTargetAt target "@/lib/auth/route" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/route" `shouldBe` Nothing
            matchTargetAt target "@/lib/auth/user/route" `shouldBe` Nothing

        it "multiple * each constrain exactly one segment" $ do
            let target = unsafeCompileTarget "@/lib/*/*"
            matchTargetAt target "@/lib/a/b" `shouldSatisfy` isJust
            matchTargetAt target "@/lib/a" `shouldBe` Nothing
            matchTargetAt target "@/lib/a/b/c" `shouldBe` Nothing

        -- \** recursive wildcard
        it "**/segment matches that segment at any depth including zero" $ do
            let target = unsafeCompileTarget "**/index"
            matchTargetAt target "index" `shouldSatisfy` isJust
            matchTargetAt target "src/index" `shouldSatisfy` isJust
            matchTargetAt target "src/app/index" `shouldSatisfy` isJust
            matchTargetAt target "src/app/deep/index" `shouldSatisfy` isJust
            matchTargetAt target "other" `shouldBe` Nothing

        it "**/segment does not match paths whose final segment only shares a suffix" $ do
            let target = unsafeCompileTarget "**/components"
            matchTargetAt target "xcomponents" `shouldBe` Nothing
            matchTargetAt target "a/xcomponents" `shouldBe` Nothing
            matchTargetAt target "components/child" `shouldBe` Nothing

        it "@/features/**/{{FileName}} at zero subdirs extracts the variable correctly" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            envZero <-
                requireJust "zero-subdir match failed" $
                    matchTargetAt target "@/features/HomeContainer"
            casingOf PascalCase envZero `shouldBe` Just "Home"
            envOne <-
                requireJust "one-subdir match failed" $
                    matchTargetAt target "@/features/auth/HomeContainer"
            casingOf PascalCase envOne `shouldBe` Just "Home"

        it "* and ** are distinct: * stops at a slash, ** crosses slashes" $ do
            let star = unsafeCompileTarget "@/lib/*"
            let globStar = unsafeCompileTarget "@/lib/**"
            matchTargetAt star "@/lib/a/b" `shouldBe` Nothing
            matchTargetAt globStar "@/lib/a/b" `shouldSatisfy` isJust

        it "extracts {{FILE_NAME}} (ConstantCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "src/constants/{{FILE_NAME}}"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "src/constants/MAX_RETRY_COUNT"
            env.targetDir `shouldBe` "src/constants"
            casingOf ConstantCase env `shouldBe` Just "MAX_RETRY_COUNT"
            casingOf PascalCase env `shouldBe` Just "MaxRetryCount"
            casingOf CamelCase env `shouldBe` Just "maxRetryCount"
            casingOf KebabCase env `shouldBe` Just "max-retry-count"

        it "rejects a path whose casing does not satisfy {{FILE_NAME}} (requires [A-Z0-9_]+)" $ do
            let target = unsafeCompileTarget "src/constants/{{FILE_NAME}}"
            matchTargetAt target "src/constants/maxRetryCount" `shouldBe` Nothing
            matchTargetAt target "src/constants/max-retry-count" `shouldBe` Nothing

        it "treats an all-uppercase captured word as a single token (HTTP -> http)" $ do
            let target = unsafeCompileTarget "@/services/{{FileName}}Client"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/services/HTTPClient"
            -- Original PascalCase capture is preserved by Map.union
            casingOf PascalCase env `shouldBe` Just "HTTP"
            -- Derived casings treat "HTTP" as one word
            casingOf KebabCase env `shouldBe` Just "http"
            casingOf CamelCase env `shouldBe` Just "http"
            casingOf ConstantCase env `shouldBe` Just "HTTP"

        it "extracts {{FileName}} with an embedded digit (OAuth2Service)" $ do
            let target = unsafeCompileTarget "@/services/{{FileName}}Service"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/services/OAuth2Service"
            casingOf PascalCase env `shouldBe` Just "OAuth2"
            casingOf CamelCase env `shouldBe` Just "oAuth2"
            casingOf KebabCase env `shouldBe` Just "o-auth2"
            casingOf ConstantCase env `shouldBe` Just "O_AUTH2"

        it "extracts {{file-name}} combined with ** at zero subdirs" $ do
            let target = unsafeCompileTarget "@/features/**/{{file-name}}-service"
            envZero <-
                requireJust "zero-subdir failed" $
                    matchTargetAt target "@/features/user-auth-service"
            envZero.targetDir `shouldBe` "@/features"
            casingOf KebabCase envZero `shouldBe` Just "user-auth"
            casingOf PascalCase envZero `shouldBe` Just "UserAuth"
            envOne <-
                requireJust "one-subdir failed" $
                    matchTargetAt target "@/features/auth/user-auth-service"
            envOne.targetDir `shouldBe` "@/features/auth"
            casingOf KebabCase envOne `shouldBe` Just "user-auth"

        it "escapes dots in literal path segments of a target pattern" $ do
            let target = unsafeCompileTarget "src/utils.lib/{{FileName}}"
            matchTargetAt target "src/utils.lib/HomeView" `shouldSatisfy` isJust
            matchTargetAt target "src/utils_lib/HomeView" `shouldBe` Nothing
            matchTargetAt target "src/utilsXlib/HomeView" `shouldBe` Nothing

        it "derives empty string as TARGET_DIR for a root-level (single-segment) file" $ do
            let target = unsafeCompileTarget "{{FileName}}"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "HomeView"
            env.targetDir `shouldBe` ""
            casingOf PascalCase env `shouldBe` Just "HomeView"

        -- TypeScript cross-casing: KebabCase ↔ PascalCase ↔ CamelCase
        it "derives all casings correctly for a three-word kebab-case name" $ do
            -- Typical TypeScript: file named user-profile-card.tsx → component UserProfileCard
            let target = unsafeCompileTarget "@/components/{{file-name}}"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/components/user-profile-card"
            casingOf KebabCase env `shouldBe` Just "user-profile-card"
            casingOf PascalCase env `shouldBe` Just "UserProfileCard"
            casingOf CamelCase env `shouldBe` Just "userProfileCard"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE_CARD"

        it "derives all casings correctly for a three-word camelCase name" $ do
            -- Typical TypeScript: service/hook named userProfileCardService
            let target = unsafeCompileTarget "@/services/{{fileName}}Service"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/services/userProfileCardService"
            casingOf CamelCase env `shouldBe` Just "userProfileCard"
            casingOf PascalCase env `shouldBe` Just "UserProfileCard"
            casingOf KebabCase env `shouldBe` Just "user-profile-card"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE_CARD"

    describe "matchClauseAt" $ do
        let sampleEnv = envFor "@/features/user/{{FileName}}" "@/features/user/UserSettings"
        let richEnv = envFor "@/features/home/{{FileName}}" "@/features/home/HomeProfile"

        it "interpolates {{TARGET_DIR}} and static strings successfully" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/data/repository"
            matchClauseAt rule sampleEnv "@/features/user/data/repository" `shouldBe` True

        it "interpolates {{file-name}} casings correctly" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/data/{{file-name}}-repository"
            matchClauseAt rule sampleEnv "@/features/user/data/user-settings-repository" `shouldBe` True

        it "rejects paths where the interpolated variables are incorrect" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/data/{{file-name}}-repository"
            -- Mismatched directory
            matchClauseAt rule sampleEnv "@/features/other/data/user-settings-repository" `shouldBe` False
            -- Wrong casing (PascalCase instead of kebab-case)
            matchClauseAt rule sampleEnv "@/features/user/data/UserSettings-repository" `shouldBe` False

        it "handles globs correctly alongside variables" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/*{{FileName}}*"
            matchClauseAt rule sampleEnv "@/features/user/components/buttons/UserSettingsButton" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/components/buttons/OtherButton" `shouldBe` False

        it "interpolates {{FileName}} (PascalCase) into a rule" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClauseAt rule richEnv "@/features/home/HomeProfileView" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/homeProfileView" `shouldBe` False

        it "interpolates {{fileName}} (camelCase) into a rule" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{fileName}}Service"
            matchClauseAt rule richEnv "@/features/home/homeProfileService" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/HomeProfileService" `shouldBe` False

        it "interpolates {{FILE_NAME}} (CONSTANT_CASE) into a rule" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FILE_NAME}}_config"
            matchClauseAt rule richEnv "@/features/home/HOME_PROFILE_config" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/home-profile_config" `shouldBe` False

        it "interpolates a literal prefix alongside {{FileName}} (use{{FileName}}ViewModel)" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            matchClauseAt rule richEnv "@/features/home/useHomeProfileViewModel" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/HomeProfileViewModel" `shouldBe` False

        it "matches a .spec existence pattern" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            matchClauseAt rule richEnv "@/features/home/useHomeProfileViewModel.spec" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/useHomeProfileViewModel.test" `shouldBe` False

        it "matches a .stories existence pattern" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View.stories"
            matchClauseAt rule richEnv "@/features/home/HomeProfileView.stories" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/HomeProfileView.spec" `shouldBe` False

        it "fails closed when a variable is absent from the environment" $ do
            -- Compilation rejects unbound variables, so this state is unreachable
            -- in practice. It must never widen a rule into matching everything.
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClauseAt rule sparseEnv "@/features/x/AnythingView" `shouldBe` False
            matchClauseAt rule sparseEnv "@/features/x/SomethingElseView" `shouldBe` False
            matchClauseAt rule sparseEnv "@/features/other/AnythingView" `shouldBe` False

        -- \* in rules
        it "* in a rule matches exactly one segment" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/*"
            matchClauseAt rule sampleEnv "@/features/user/anything" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/a/b" `shouldBe` False
            matchClauseAt rule sampleEnv "@/features/other/anything" `shouldBe` False

        it "* in the middle of a rule does not cross a path separator" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/*/index"
            matchClauseAt rule sampleEnv "@/features/user/components/index" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/a/b/index" `shouldBe` False
            matchClauseAt rule sampleEnv "@/features/user/index" `shouldBe` False

        -- \** in rules
        it "{{TARGET_DIR}}/**/* matches zero subdirs (rule regression)" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/*"
            matchClauseAt rule sampleEnv "@/features/user/Button" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/components/Button" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/other/Button" `shouldBe` False

        it "{{TARGET_DIR}}/** matches any path at any depth below TARGET_DIR" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**"
            matchClauseAt rule sampleEnv "@/features/user/anything" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/a/b/c" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/other/anything" `shouldBe` False

        it "{{TARGET_DIR}}/**/*.spec matches .spec files at any depth including zero subdirs" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/*.spec"
            matchClauseAt rule sampleEnv "@/features/user/Button.spec" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/components/Button.spec" `shouldBe` True
            matchClauseAt rule sampleEnv "@/features/user/Button.test" `shouldBe` False
            matchClauseAt rule sampleEnv "@/features/other/Button.spec" `shouldBe` False

        it "{{TARGET_DIR}}/**/{{FileName}}.spec matches at any depth including zero subdirs" $ do
            let rule = unsafeCompileClause "{{TARGET_DIR}}/**/{{FileName}}.spec"
            matchClauseAt rule richEnv "@/features/home/HomeProfile.spec" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/auth/HomeProfile.spec" `shouldBe` True
            matchClauseAt rule richEnv "@/features/home/HomeProfile.test" `shouldBe` False
            matchClauseAt rule richEnv "@/features/other/HomeProfile.spec" `shouldBe` False

        it "escapes regex metacharacters in TARGET_DIR (dot must not match arbitrary chars)" $ do
            let env = envFor "src/v1.0/features/{{FileName}}" "src/v1.0/features/Home"
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClauseAt rule env "src/v1.0/features/HomeView" `shouldBe` True
            matchClauseAt rule env "src/v1X0/features/HomeView" `shouldBe` False

    describe "moduleFromGlob" $ do
        let env = envFor "@/features/auth/{{FileName}}" "@/features/auth/UserAuth"

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

        it "returns Nothing when a variable is absent from the env" $ do
            -- No concrete module can be named, so none is claimed to exist.
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            moduleFromGlob sparseEnv pat `shouldBe` Nothing

        describe "with several variables" $ do
            let providerTarget = "@/components/{{provider-name}}/{{service-type}}/{{FileName}}View"
            let providerScope = (unsafeCompileTarget providerTarget).boundVars
            let expand = moduleFromGlob (envFor providerTarget "@/components/stripe-connect/payment/CheckoutView")
            let clause = unsafeCompileClauseIn providerScope

            it "expands each variable in the casing the clause asked for" $
                expand (clause "@/services/{{provider-name}}/{{ServiceType}}{{FileName}}Client")
                    `shouldBe` Just "@/services/stripe-connect/PaymentCheckoutClient"

            it "expands one variable into all four casings at once" $
                expand (clause "@/x/{{ProviderName}}/{{providerName}}/{{provider-name}}/{{PROVIDER_NAME}}")
                    `shouldBe` Just "@/x/StripeConnect/stripeConnect/stripe-connect/STRIPE_CONNECT"

            it "combines several variables with TARGET_DIR" $
                expand (clause "{{TARGET_DIR}}/{{ServiceType}}-{{file-name}}.spec")
                    `shouldBe` Just "@/components/stripe-connect/payment/Payment-checkout.spec"

            it "still refuses when any part of the pattern is a wildcard" $
                expand (clause "{{TARGET_DIR}}/{{ServiceType}}/*") `shouldBe` Nothing

        describe "expanding a name that was captured as an acronym" $ do
            let widgetOf = envFor "@/widgets/{{FileName}}Widget"
            let configOf path = moduleFromGlob (widgetOf path) (unsafeCompileClause "@/config/{{file-name}}")

            it "reads a run of capitals as one word" $ do
                configOf "@/widgets/DBConnectionWidget" `shouldBe` Just "@/config/db-connection"
                configOf "@/widgets/HTTPClientWidget" `shouldBe` Just "@/config/http-client"

            it "names a module that cannot exist for two adjacent acronyms - a documented limitation" $
                configOf "@/widgets/AWSS3Widget" `shouldBe` Just "@/config/awss3"

            it "keeps the captured spelling when the clause asks for the same casing" $ do
                let pascalOf path = moduleFromGlob (widgetOf path) (unsafeCompileClause "@/config/{{FileName}}")
                pascalOf "@/widgets/AWSS3Widget" `shouldBe` Just "@/config/AWSS3"
                pascalOf "@/widgets/DBConnectionWidget" `shouldBe` Just "@/config/DBConnection"

            it "is exact when the target also names the folder in kebab-case" $ do
                let paired = "@/components/{{provider-name}}/{{ProviderName}}View"
                let pairedScope = (unsafeCompileTarget paired).boundVars
                let pairedEnv = envFor paired "@/components/aws-s3/AWSS3View"
                moduleFromGlob pairedEnv (unsafeCompileClauseIn pairedScope "@/config/{{provider-name}}")
                    `shouldBe` Just "@/config/aws-s3"
                moduleFromGlob pairedEnv (unsafeCompileClauseIn pairedScope "@/config/{{ProviderName}}")
                    `shouldBe` Just "@/config/AWSS3"

        describe "expanding a name of three or more words" $ do
            let useCaseTarget = "@/application/{{use-case-name}}/{{UseCaseName}}UseCase"
            let useCaseScope = (unsafeCompileTarget useCaseTarget).boundVars
            let expand = moduleFromGlob (envFor useCaseTarget "@/application/archive-order/ArchiveOrderUseCase")

            it "expands a three-word name between a prefix and a suffix" $
                expand (unsafeCompileClauseIn useCaseScope "{{TARGET_DIR}}/use{{UseCaseName}}ViewModel")
                    `shouldBe` Just "@/application/archive-order/useArchiveOrderViewModel"

            it "expands a three-word name into every casing" $
                expand (unsafeCompileClauseIn useCaseScope "@/x/{{UseCaseName}}/{{useCaseName}}/{{use-case-name}}/{{USE_CASE_NAME}}")
                    `shouldBe` Just "@/x/ArchiveOrder/archiveOrder/archive-order/ARCHIVE_ORDER"

    describe "renderClausePattern" $ do
        let env = envFor "@/features/auth/{{FileName}}" "@/features/auth/UserAuth"

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

        it "falls back to the variable name when it is absent from the env" $ do
            let pat = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            renderClausePattern sparseEnv pat `shouldBe` "@/features/x/{{FileName}}View"

    describe "interpolate" $ do
        let env = envFor "@/features/auth/{{FileName}}" "@/features/auth/UserAuth"

        it "substitutes a variable in each of the four casings" $ do
            interpolate env "{{FileName}} {{fileName}} {{file-name}} {{FILE_NAME}}"
                `shouldBe` "UserAuth userAuth user-auth USER_AUTH"

        it "substitutes TARGET_DIR" $
            interpolate env "Add a spec next to {{TARGET_DIR}}."
                `shouldBe` "Add a spec next to @/features/auth."

        it "substitutes a variable that occurs more than once" $
            interpolate env "Import use{{FileName}}ViewModel and drive {{FileName}}View from it."
                `shouldBe` "Import useUserAuthViewModel and drive UserAuthView from it."

        it "leaves prose without variables untouched" $
            interpolate env "Promote the shared code out of the provider folders."
                `shouldBe` "Promote the shared code out of the provider folders."

        it "leaves the empty text untouched" $
            interpolate env "" `shouldBe` ""

        it "keeps wildcards and slashes literal, because prose is not a pattern" $
            interpolate env "Move **/*.spec files under {{TARGET_DIR}}/tests."
                `shouldBe` "Move **/*.spec files under @/features/auth/tests."

        it "leaves a misspelled variable exactly as written" $
            interpolate env "Add a {{FileNam}}View." `shouldBe` "Add a {{FileNam}}View."

        it "leaves a variable the target never bound exactly as written" $
            interpolate env "Import {{provider-name}}." `shouldBe` "Import {{provider-name}}."

        it "leaves a token that is not written in a recognised casing as written" $
            interpolate env "Rename {{File_Name}}." `shouldBe` "Rename {{File_Name}}."

        it "leaves a whitespace-padded token as written" $
            interpolate env "Add {{ FileName }}." `shouldBe` "Add {{ FileName }}."

        it "leaves a token containing pattern syntax as written" $
            interpolate env "See {{a/b}} and {{a*b}}." `shouldBe` "See {{a/b}} and {{a*b}}."

        it "leaves empty braces as written" $
            interpolate env "Braces are written {{}}." `shouldBe` "Braces are written {{}}."

        it "leaves unclosed braces as written" $
            interpolate env "An unfinished {{FileName is just prose."
                `shouldBe` "An unfinished {{FileName is just prose."

        it "still finds a variable nested inside an unrecognised token" $
            interpolate env "{{outer {{FileName}}" `shouldBe` "{{outer UserAuth"

        it "substitutes every variable when the target binds several" $ do
            let providerEnv =
                    envFor
                        "@/components/{{provider-name}}/{{service-type}}/{{FileName}}View"
                        "@/components/stripe-connect/payment/CheckoutView"
            interpolate providerEnv "Import @/services/{{provider-name}}/{{service-type}}-{{file-name}} from {{TARGET_DIR}}."
                `shouldBe` "Import @/services/stripe-connect/payment-checkout from @/components/stripe-connect/payment."

        it "substitutes TARGET_DIR even when the env binds no variables" $
            interpolate sparseEnv "Look in {{TARGET_DIR}} for {{FileName}}."
                `shouldBe` "Look in @/features/x for {{FileName}}."

    describe "End-to-End Scenarios" $ do
        it "validates the Page Architecture ViewModel rule end-to-end" $ do
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cRule = unsafeCompileClause "{{TARGET_DIR}}/data/{{file-name}}-repository"
            let targetPath = "@/features/auth/useUserAuthViewModel"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt cTarget targetPath

            matchClauseAt cRule env "@/features/auth/data/user-auth-repository" `shouldBe` True
            matchClauseAt cRule env "@/features/auth/data/global-repository" `shouldBe` False
            matchClauseAt cRule env "@/features/other/data/user-auth-repository" `shouldBe` False

        it "validates the Container wires View and ViewModel (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            let cStateEvent = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}StateEvent"
            let cViewModel = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel"
            let cView = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            let targetPath = "@/features/checkout/PaymentContainer"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt cTarget targetPath

            matchClauseAt cStateEvent env "@/features/checkout/PaymentStateEvent" `shouldBe` True
            matchClauseAt cViewModel env "@/features/checkout/usePaymentViewModel" `shouldBe` True
            matchClauseAt cView env "@/features/checkout/PaymentView" `shouldBe` True
            -- Wrong feature dir
            matchClauseAt cStateEvent env "@/features/home/PaymentStateEvent" `shouldBe` False
            -- Wrong component name
            matchClauseAt cViewModel env "@/features/checkout/useCheckoutViewModel" `shouldBe` False

        it "validates the ViewModel test existence rule (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cSpec = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
            let targetPath = "@/features/auth/useUserAuthViewModel"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt cTarget targetPath

            matchClauseAt cSpec env "@/features/auth/useUserAuthViewModel.spec" `shouldBe` True
            matchClauseAt cSpec env "@/features/auth/useUserAuthViewModel.test" `shouldBe` False
            matchClauseAt cSpec env "@/features/other/useUserAuthViewModel.spec" `shouldBe` False

        it "validates the View Storybook existence rule (page-architecture)" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}View"
            let cStories = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View.stories"
            let targetPath = "@/features/profile/UserProfileView"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt cTarget targetPath

            matchClauseAt cStories env "@/features/profile/UserProfileView.stories" `shouldBe` True
            matchClauseAt cStories env "@/features/profile/UserProfileView.storybook" `shouldBe` False
            matchClauseAt cStories env "@/features/profile/UserProfileView.spec" `shouldBe` False

        it "validates the ViewModel forbids-import rule (page-architecture)" $ do
            -- ViewModel must NOT import its own View; matchClauseAt True = forbids path detected
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cforbids = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            let targetPath = "@/features/home/useHomeViewModel"
            env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt cTarget targetPath

            matchClauseAt cforbids env "@/features/home/HomeView" `shouldBe` True
            -- Other paths in the same dir are not caught by this forbids rule
            matchClauseAt cforbids env "@/features/home/HomeContainer" `shouldBe` False
            matchClauseAt cforbids env "@/features/other/HomeView" `shouldBe` False

        it "validates a ConstantCase naming convention end-to-end" $ do
            -- Cross-casing from ConstantCase is lossy (see matchTargetAt tests above),
            -- so same-casing enforcement ({{FILE_NAME}} -> {{FILE_NAME}}) is reliable.
            let cTarget = unsafeCompileTarget "src/constants/{{FILE_NAME}}"
            let cRule = unsafeCompileClause "src/types/{{FILE_NAME}}_types"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt cTarget "src/constants/MAX_RETRY_COUNT"
            -- same casing is preserved exactly, so same-style rules match correctly
            matchClauseAt cRule env "src/types/MAX_RETRY_COUNT_types" `shouldBe` True
            -- wrong constant name
            matchClauseAt cRule env "src/types/MIN_RETRY_COUNT_types" `shouldBe` False
            -- wrong directory
            matchClauseAt cRule env "src/constants/MAX_RETRY_COUNT_types" `shouldBe` False

    describe "TypeScript web codebase patterns" $ do
        -- PascalCase → KebabCase: the canonical React pattern
        it "PascalCase component enforces kebab-case CSS module" $ do
            let cTarget = unsafeCompileTarget "@/components/{{FileName}}"
            let cCssModule = unsafeCompileClause "{{TARGET_DIR}}/{{file-name}}.module.css"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt cTarget "@/components/UserProfileCard"
            matchClauseAt cCssModule env "@/components/user-profile-card.module.css" `shouldBe` True
            -- PascalCase CSS module name is wrong
            matchClauseAt cCssModule env "@/components/UserProfileCard.module.css" `shouldBe` False
            -- Partial name mismatch
            matchClauseAt cCssModule env "@/components/user-profile.module.css" `shouldBe` False

        it "PascalCase component enforces PascalCase stories and spec" $ do
            let cTarget = unsafeCompileTarget "@/features/**/{{FileName}}"
            let cStories = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}.stories"
            let cSpec = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}.spec"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt cTarget "@/features/auth/LoginForm"
            matchClauseAt cStories env "@/features/auth/LoginForm.stories" `shouldBe` True
            matchClauseAt cSpec env "@/features/auth/LoginForm.spec" `shouldBe` True
            -- Kebab-case versions of stories/spec are wrong
            matchClauseAt cStories env "@/features/auth/login-form.stories" `shouldBe` False
            matchClauseAt cSpec env "@/features/auth/login-form.spec" `shouldBe` False

        -- KebabCase → PascalCase: the reverse cross-casing direction
        it "kebab-case file target enforces PascalCase component and camelCase hook rules" $ do
            let cTarget = unsafeCompileTarget "@/components/{{file-name}}"
            let cComponent = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}"
            let cHook = unsafeCompileClause "{{TARGET_DIR}}/use{{FileName}}"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt cTarget "@/components/login-form"
            matchClauseAt cComponent env "@/components/LoginForm" `shouldBe` True
            matchClauseAt cHook env "@/components/useLoginForm" `shouldBe` True
            -- Kebab casing is wrong in a PascalCase rule slot
            matchClauseAt cComponent env "@/components/login-form" `shouldBe` False
            -- Capital "Use" is wrong (hook prefix is camelCase)
            matchClauseAt cHook env "@/components/UseLoginForm" `shouldBe` False

        -- CamelCase → PascalCase + KebabCase: TypeScript service/interface convention
        it "camelCase service target enforces PascalCase interface and kebab-case spec" $ do
            let cTarget = unsafeCompileTarget "@/services/{{fileName}}Service"
            let cInterface = unsafeCompileClause "{{TARGET_DIR}}/I{{FileName}}Service"
            let cSpec = unsafeCompileClause "{{TARGET_DIR}}/{{file-name}}-service.spec"
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt cTarget "@/services/userProfileService"
            matchClauseAt cInterface env "@/services/IUserProfileService" `shouldBe` True
            matchClauseAt cSpec env "@/services/user-profile-service.spec" `shouldBe` True
            -- Wrong casing for interface (lowercase 'i' prefix or wrong name form)
            matchClauseAt cInterface env "@/services/userProfileService" `shouldBe` False
            -- PascalCase spec file name is wrong
            matchClauseAt cSpec env "@/services/UserProfileService.spec" `shouldBe` False

    multiVariableSpec
    multiWordNameSpec
    capturePositionSpec
    casingAgreementSpec
    polaritySpec
    compilationErrorSpec
    globPlusProps
    globPlusModelProps

--------------------------------------------------------------------------------
-- Multiple variables
--------------------------------------------------------------------------------

multiVariableSpec :: Spec
multiVariableSpec = describe "multiple variables" $ do
    let providerTarget = unsafeCompileTarget "@/components/{{provider-name}}/{{service-type}}/{{FileName}}View"
    let providerScope = providerTarget.boundVars

    it "captures every variable in a target pattern" $ do
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt providerTarget "@/components/stripe-connect/payment/CheckoutView"

        env.targetDir `shouldBe` "@/components/stripe-connect/payment"
        varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
        varOf "service-type" KebabCase env `shouldBe` Just "payment"
        varOf "file-name" PascalCase env `shouldBe` Just "Checkout"

    it "enriches each variable independently into all four casings" $ do
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt providerTarget "@/components/stripe-connect/payment/CheckoutView"

        varOf "provider-name" PascalCase env `shouldBe` Just "StripeConnect"
        varOf "provider-name" CamelCase env `shouldBe` Just "stripeConnect"
        varOf "provider-name" ConstantCase env `shouldBe` Just "STRIPE_CONNECT"
        varOf "service-type" PascalCase env `shouldBe` Just "Payment"
        varOf "file-name" KebabCase env `shouldBe` Just "checkout"

    it "matches a clause that mixes several variables in different casings" $ do
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt providerTarget "@/components/stripe-connect/payment/CheckoutView"
        let clause = unsafeCompileClauseIn providerScope "@/services/{{provider-name}}/{{ServiceType}}{{FileName}}Client"

        matchClauseAt clause env "@/services/stripe-connect/PaymentCheckoutClient" `shouldBe` True
        matchClauseAt clause env "@/services/paypal/PaymentCheckoutClient" `shouldBe` False
        matchClauseAt clause env "@/services/stripe-connect/PayoutCheckoutClient" `shouldBe` False

    it "expands several variables into a concrete module path" $ do
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt providerTarget "@/components/stripe-connect/payment/CheckoutView"
        let clause = unsafeCompileClauseIn providerScope "{{TARGET_DIR}}/{{provider-name}}-{{service-type}}-{{file-name}}"

        moduleFromGlob env clause
            `shouldBe` Just "@/components/stripe-connect/payment/stripe-connect-payment-checkout"

    it "renders unbound variables by name rather than as {{FileName}}" $ do
        let clause = unsafeCompileClauseIn providerScope "{{TARGET_DIR}}/{{provider-name}}/{{ServiceType}}"
        renderClausePattern sparseEnv clause
            `shouldBe` "@/features/x/{{provider-name}}/{{ServiceType}}"

    it "does not match when a variable's casing is violated" $ do
        matchTargetAt providerTarget "@/components/StripeConnect/payment/CheckoutView" `shouldBe` Nothing
        matchTargetAt providerTarget "@/components/stripe-connect/payment/checkoutView" `shouldBe` Nothing

    describe "a repeated variable" $ do
        let repeated = unsafeCompileTarget "@/components/{{provider-name}}/{{ProviderName}}View"

        it "binds once when every occurrence agrees" $ do
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt repeated "@/components/stripe-connect/StripeConnectView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
            varOf "provider-name" PascalCase env `shouldBe` Just "StripeConnect"
            repeated.boundVars `shouldBe` Set.singleton (VarName "provider-name")

        it "does not match when the occurrences disagree" $ do
            matchTargetAt repeated "@/components/stripe-connect/PaypalView" `shouldBe` Nothing

        it "constrains two segments to the same value in one casing" $ do
            let sameTwice = unsafeCompileTarget "@/{{provider-name}}/{{provider-name}}-service"
            matchTargetAt sameTwice "@/stripe/stripe-service" `shouldSatisfy` isJust
            matchTargetAt sameTwice "@/stripe/paypal-service" `shouldBe` Nothing

    it "binds the leftmost variable greedily when a separator is consumable" $ do
        -- Both variables are kebab-case and '-' is a kebab character, so the
        -- boundary is ambiguous. POSIX longest-match settles it: documented,
        -- not accidental.
        let greedy = unsafeCompileTarget "@/x/{{provider-name}}-{{service-type}}"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt greedy "@/x/stripe-connect-payment-service"

        varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect-payment"
        varOf "service-type" KebabCase env `shouldBe` Just "service"

--------------------------------------------------------------------------------
-- Names of three or more words
--------------------------------------------------------------------------------

{- | Two words is the smallest legal name and the one every example reaches
for. Real rulebooks carry longer ones - @{{use-case-name}}@ has three - and a
longer name has more word boundaries to lose and more acronym readings to
confuse.
-}
multiWordNameSpec :: Spec
multiWordNameSpec = describe "a name of three or more words" $ do
    it "treats all four spellings as one variable" $ do
        let bound spelling = (.boundVars) <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{UseCaseName}}" `shouldBe` bound "{{useCaseName}}"
        bound "{{UseCaseName}}" `shouldBe` bound "{{use-case-name}}"
        bound "{{UseCaseName}}" `shouldBe` bound "{{USE_CASE_NAME}}"
        bound "{{UseCaseName}}" `shouldBe` Right (Set.singleton (VarName "use-case-name"))

    it "treats all four spellings of a five-word name as one variable" $ do
        let bound spelling = (.boundVars) <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{UserProfileSettingsPageTitle}}" `shouldBe` bound "{{user-profile-settings-page-title}}"
        bound "{{UserProfileSettingsPageTitle}}" `shouldBe` bound "{{USER_PROFILE_SETTINGS_PAGE_TITLE}}"

    it "captures a three-word name between a prefix and a suffix" $ do
        let target = unsafeCompileTarget "@/application/use{{UseCaseName}}ViewModel"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/application/useArchiveOrderViewModel"

        varOf "use-case-name" PascalCase env `shouldBe` Just "ArchiveOrder"
        varOf "use-case-name" KebabCase env `shouldBe` Just "archive-order"
        varOf "use-case-name" CamelCase env `shouldBe` Just "archiveOrder"
        varOf "use-case-name" ConstantCase env `shouldBe` Just "ARCHIVE_ORDER"

    it "does not match when the suffix around the variable differs" $ do
        let target = unsafeCompileTarget "@/application/use{{UseCaseName}}ViewModel"
        matchTargetAt target "@/application/useArchiveOrderContainer" `shouldBe` Nothing
        matchTargetAt target "@/application/getArchiveOrderViewModel" `shouldBe` Nothing

    it "binds greedily when the captured value repeats the suffix" $ do
        let target = unsafeCompileTarget "@/application/use{{UseCaseName}}ViewModel"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/application/useArchiveViewModelViewModel"

        varOf "use-case-name" PascalCase env `shouldBe` Just "ArchiveViewModel"

    it "carries a three-word name across casings into a clause" $ do
        let target = unsafeCompileTarget "@/application/{{use-case-name}}/use{{UseCaseName}}ViewModel"
        let scope = target.boundVars
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/application/archive-order/useArchiveOrderViewModel"
        let clause = unsafeCompileClauseIn scope "{{TARGET_DIR}}/{{UseCaseName}}UseCase"

        matchClauseAt clause env "@/application/archive-order/ArchiveOrderUseCase" `shouldBe` True
        matchClauseAt clause env "@/application/archive-order/ArchiveUseCase" `shouldBe` False
        moduleFromGlob env clause
            `shouldBe` Just "@/application/archive-order/ArchiveOrderUseCase"

    it "expands a three-word name into every casing at once" $ do
        let target = unsafeCompileTarget "@/application/{{UseCaseName}}UseCase"
        let scope = target.boundVars
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/application/ArchiveOrderUseCase"
        let everyCasing = "@/x/{{UseCaseName}}/{{useCaseName}}/{{use-case-name}}/{{USE_CASE_NAME}}"
        let clause = unsafeCompileClauseIn scope everyCasing

        moduleFromGlob env clause
            `shouldBe` Just "@/x/ArchiveOrder/archiveOrder/archive-order/ARCHIVE_ORDER"

    it "agrees across casings when a three-word name carries an acronym" $ do
        let target = unsafeCompileTarget "@/x/{{use-case-name}}/{{UseCaseName}}UseCase"
        matchTargetAt target "@/x/archive-db-order/ArchiveDBOrderUseCase" `shouldSatisfy` isJust
        matchTargetAt target "@/x/http-client-pool/HTTPClientPoolUseCase" `shouldSatisfy` isJust
        matchTargetAt target "@/x/archive-db-order/ArchiveOrderUseCase" `shouldBe` Nothing

    it "repeats a three-word variable across a **/" $ do
        let target = unsafeCompileTarget "@/application/{{use-case-name}}/**/{{UseCaseName}}UseCase"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/application/archive-order/nested/ArchiveOrderUseCase"

        varOf "use-case-name" KebabCase env `shouldBe` Just "archive-order"
        varOf "use-case-name" PascalCase env `shouldBe` Just "ArchiveOrder"

    it "rejects a three-word name that is not written in one casing" $ do
        errorOf (compileTargetPattern "@/x/{{Use-Case-Name}}") `shouldBe` Just (UnrecognisedCasing "Use-Case-Name")
        errorOf (compileTargetPattern "@/x/{{use_case_name}}") `shouldBe` Just (UnrecognisedCasing "use_case_name")
        errorOf (compileTargetPattern "@/x/{{UseCASEName}}") `shouldBe` Just (ConsecutiveCapitals "UseCASEName")

--------------------------------------------------------------------------------
-- Capture positions
--------------------------------------------------------------------------------

{- | A target's regex numbers its groups by the position of their opening paren,
so the @**\/@ idiom's group interleaves with the variable groups rather than
preceding them. Every case here puts a variable /before/ a @**\/@, which is the
arrangement the original suite never generated.
-}
capturePositionSpec :: Spec
capturePositionSpec = describe "capture positions" $ do
    describe "a variable before a **/" $ do
        let target = unsafeCompileTarget "@/components/{{provider-name}}/**/{{FileName}}View"

        it "binds the variable to its own segment, not to the globstar's text" $ do
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/components/stripe-connect/payment/CheckoutView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
            varOf "file-name" PascalCase env `shouldBe` Just "Checkout"

        it "keeps providers distinct rather than collapsing them onto the globstar" $ do
            paypal <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/components/paypal/payment/RefundView"
            stripe <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/components/stripe-connect/payment/CheckoutView"

            varOf "provider-name" KebabCase paypal `shouldNotBe` varOf "provider-name" KebabCase stripe

        it "binds the variable when the globstar matches zero directories" $ do
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/components/stripe-connect/CheckoutView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"

        it "never binds a variable to text containing a path separator" $ do
            env <-
                requireJust "matchTargetAt returned Nothing" $
                    matchTargetAt target "@/components/stripe-connect/payment/payout/CheckoutView"

            varOf "provider-name" KebabCase env `shouldNotSatisfy` any (T.isInfixOf "/")

    it "matches a repeated variable that straddles a **/" $ do
        let target = unsafeCompileTarget "@/components/{{provider-name}}/**/{{ProviderName}}View"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/components/stripe-connect/payment/StripeConnectView"

        varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
        varOf "provider-name" PascalCase env `shouldBe` Just "StripeConnect"

    it "rejects a variable standing between two globstars" $ do
        -- This shape used to compile and bind service-type to whichever
        -- directory happened to sit last, which made one pattern mean
        -- different things in a shallow tree and a deep one.
        errorOf (compileTargetPattern "@/{{provider-name}}/**/{{service-type}}/**/{{FileName}}View")
            `shouldBe` Just (UnanchoredVariable (VarName "service-type"))

    it "binds a variable that follows a trailing **" $ do
        -- A trailing ** is not the **/ idiom, so it introduces no extra group.
        -- This shape worked by accident before; it must keep working.
        let target = unsafeCompileTarget "@/components/{{provider-name}}/**"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt target "@/components/stripe-connect/payment/CheckoutView"

        varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"

--------------------------------------------------------------------------------
-- Casing agreement between occurrences
--------------------------------------------------------------------------------

{- | Two spellings denote the same variable when some name could have produced
both. A run of capitals carries no word boundary, so @HTTPClient@ and
@http-client@ are the same name and must agree.
-}
casingAgreementSpec :: Spec
casingAgreementSpec = describe "casing agreement" $ do
    let repeated = unsafeCompileTarget "@/components/{{provider-name}}/{{ProviderName}}View"
    let matchesFolder path = matchTargetAt repeated path `shouldSatisfy` isJust
    let rejectsFolder path = matchTargetAt repeated path `shouldBe` Nothing

    it "agrees when a PascalCase occurrence spells a word as an acronym" $ do
        matchesFolder "@/components/http-client/HTTPClientView"
        matchesFolder "@/components/db-connection/DBConnectionView"

    it "agrees when both acronym words run together" $
        matchesFolder "@/components/aws-s3/AWSS3View"

    it "agrees on the control spellings that never had acronyms" $ do
        matchesFolder "@/components/http-cache/HttpCacheView"
        matchesFolder "@/components/stripe-connect/StripeConnectView"

    it "agrees when a word contains or begins with a digit" $ do
        matchesFolder "@/components/v2-api/V2APIView"
        matchesFolder "@/components/api-2fa/Api2faView"
        matchesFolder "@/components/http2-client/Http2ClientView"

    it "agrees when every word is a single letter" $
        matchesFolder "@/components/a-b/ABView"

    it "still rejects occurrences that no single name could have produced" $ do
        rejectsFolder "@/components/stripe-connect/PaypalView"
        rejectsFolder "@/components/http-client/HttpCacheView"

    it "agrees between CONSTANT_CASE and PascalCase across a digit" $ do
        let constantTarget = unsafeCompileTarget "@/x/{{PROVIDER_NAME}}/{{ProviderName}}View"
        matchTargetAt constantTarget "@/x/HTTP2_CLIENT/Http2ClientView" `shouldSatisfy` isJust

    describe "expansion into a casing that was never captured" $ do
        let widget = unsafeCompileTarget "@/widgets/{{FileName}}Widget"
        let kebabOf path = do
                env <- requireJust "matchTargetAt returned Nothing" $ matchTargetAt widget path
                pure (varOf "file-name" KebabCase env)

        it "reads a run of capitals as one word" $ do
            kebabOf "@/widgets/DBConnectionWidget" >>= (`shouldBe` Just "db-connection")
            kebabOf "@/widgets/HTTPClientWidget" >>= (`shouldBe` Just "http-client")

        it "leaves a single-cased name untouched" $
            kebabOf "@/widgets/UserProfileWidget" >>= (`shouldBe` Just "user-profile")

        it "cannot split two adjacent acronym words - a documented limitation" $
            kebabOf "@/widgets/AWSS3Widget" >>= (`shouldBe` Just "awss3")

        it "reads single letters as one word - a documented limitation" $
            kebabOf "@/widgets/ABTestWidget" >>= (`shouldBe` Just "ab-test")

--------------------------------------------------------------------------------
-- Polarity
--------------------------------------------------------------------------------

{- | Writing a name out in a casing it was not captured in is a guess. Which
way it is safe to guess wrong depends on what a match means, so a @forbids:@
clause accepts every spelling of the name and the rest accept only the
canonical one.
-}
polaritySpec :: Spec
polaritySpec = describe "polarity" $ do
    let target = unsafeCompileTarget "@/widgets/{{file-name}}"
    let scope = target.boundVars
    let forbidding = unsafeCompileClauseAs Widen scope "@/internal/{{FileName}}/**"
    let requiring = unsafeCompileClauseAs Narrow scope "@/internal/{{FileName}}/**"
    let envOf = envFor "@/widgets/{{file-name}}"

    it "accepts every acronym spelling in a forbidding clause" $ do
        let env = envOf "@/widgets/db-connection"
        matchClauseAt forbidding env "@/internal/DbConnection/x" `shouldBe` True
        matchClauseAt forbidding env "@/internal/DBConnection/x" `shouldBe` True

    it "accepts only the canonical spelling in a requiring clause" $ do
        let env = envOf "@/widgets/db-connection"
        matchClauseAt requiring env "@/internal/DbConnection/x" `shouldBe` True
        matchClauseAt requiring env "@/internal/DBConnection/x" `shouldBe` False

    it "accepts every reading of an ambiguous capture in a forbidding clause" $ do
        -- ABTest could be ["ab","test"] or ["a","b","test"]; a forbidding
        -- clause must not let the reading it did not pick slip through.
        let env = envFor "@/widgets/{{FileName}}" "@/widgets/ABTest"
        let forbidsKebab = unsafeCompileClauseAs Widen (Set.singleton (VarName "file-name")) "@/internal/{{file-name}}/**"
        matchClauseAt forbidsKebab env "@/internal/ab-test/x" `shouldBe` True
        matchClauseAt forbidsKebab env "@/internal/a-b-test/x" `shouldBe` True

    it "does not widen a casing that has only one spelling" $ do
        let env = envOf "@/widgets/db-connection"
        let kebabClause polarity = unsafeCompileClauseAs polarity scope "@/internal/{{file-name}}/**"
        matchClauseAt (kebabClause Widen) env "@/internal/db-connection/x" `shouldBe` True
        matchClauseAt (kebabClause Widen) env "@/internal/dbConnection/x" `shouldBe` False

    describe "several variables in one clause" $ do
        let providerTarget = "@/components/{{provider-name}}/{{service-type}}/{{FileName}}View"
        let providerScope = (unsafeCompileTarget providerTarget).boundVars
        let clauseOf polarity = unsafeCompileClauseAs polarity providerScope "@/x/{{ProviderName}}/{{ServiceType}}/{{file-name}}"
        let env = envFor providerTarget "@/components/stripe-connect/payment/CheckoutView"

        it "requires every variable to line up, whatever the polarity" $
            for_ [Narrow, Widen] $ \polarity -> do
                matchClauseAt (clauseOf polarity) env "@/x/StripeConnect/Payment/checkout" `shouldBe` True
                -- one variable wrong at a time
                matchClauseAt (clauseOf polarity) env "@/x/Paypal/Payment/checkout" `shouldBe` False
                matchClauseAt (clauseOf polarity) env "@/x/StripeConnect/Payout/checkout" `shouldBe` False
                matchClauseAt (clauseOf polarity) env "@/x/StripeConnect/Payment/refund" `shouldBe` False

        it "widens every variable independently in a forbidding clause" $ do
            matchClauseAt (clauseOf Widen) env "@/x/STRIPEConnect/Payment/checkout" `shouldBe` True
            matchClauseAt (clauseOf Widen) env "@/x/StripeConnect/PAYMENT/checkout" `shouldBe` True
            matchClauseAt (clauseOf Widen) env "@/x/STRIPECONNECT/PAYMENT/checkout" `shouldBe` True

        it "narrows every variable in a requiring clause" $ do
            matchClauseAt (clauseOf Narrow) env "@/x/STRIPEConnect/Payment/checkout" `shouldBe` False
            matchClauseAt (clauseOf Narrow) env "@/x/StripeConnect/PAYMENT/checkout" `shouldBe` False

        it "widens an acronym-bound variable alongside a plain one" $ do
            let acronymEnv = envFor providerTarget "@/components/http-client/db-sync/AWSS3View"
            let acronymClause polarity = unsafeCompileClauseAs polarity providerScope "@/x/{{ProviderName}}/{{ServiceType}}/{{file-name}}"

            -- The kebab folders pin two names exactly; the Pascal file name is
            -- read as one acronym word, which is the documented guess.
            matchClauseAt (acronymClause Narrow) acronymEnv "@/x/HttpClient/DbSync/awss3" `shouldBe` True
            matchClauseAt (acronymClause Widen) acronymEnv "@/x/HTTPClient/DBSync/awss3" `shouldBe` True
            matchClauseAt (acronymClause Narrow) acronymEnv "@/x/HTTPClient/DBSync/awss3" `shouldBe` False

        it "keeps a variable adjacent to another exact under both polarities" $ do
            let adjacent polarity = unsafeCompileClauseAs polarity providerScope "@/x/{{ServiceType}}{{FileName}}"
            for_ [Narrow, Widen] $ \polarity ->
                matchClauseAt (adjacent polarity) env "@/x/PaymentCheckout" `shouldBe` True
            matchClauseAt (adjacent Widen) env "@/x/PAYMENTCheckout" `shouldBe` True
            matchClauseAt (adjacent Narrow) env "@/x/PAYMENTCheckout" `shouldBe` False

        it "combines several variables with TARGET_DIR and a glob" $ do
            let mixed polarity = unsafeCompileClauseAs polarity providerScope "{{TARGET_DIR}}/**/{{ServiceType}}-{{file-name}}.spec"
            for_ [Narrow, Widen] $ \polarity -> do
                matchClauseAt (mixed polarity) env "@/components/stripe-connect/payment/deep/Payment-checkout.spec" `shouldBe` True
                matchClauseAt (mixed polarity) env "@/components/paypal/payment/Payment-checkout.spec" `shouldBe` False

    it "keeps the literal capture exact under both polarities" $ do
        let pascalTarget = unsafeCompileTarget "@/widgets/{{FileName}}"
        let pascalScope = pascalTarget.boundVars
        let clause polarity = unsafeCompileClauseAs polarity pascalScope "@/internal/{{FileName}}/**"
        env <-
            requireJust "matchTargetAt returned Nothing" $
                matchTargetAt pascalTarget "@/widgets/DBConnection"

        matchClauseAt (clause Narrow) env "@/internal/DBConnection/x" `shouldBe` True
        matchClauseAt (clause Widen) env "@/internal/DBConnection/x" `shouldBe` True

--------------------------------------------------------------------------------
-- Compilation errors
--------------------------------------------------------------------------------

compilationErrorSpec :: Spec
compilationErrorSpec = describe "compilation errors" $ do
    let fileName = Set.singleton (VarName "file-name")

    it "rejects a single-word name that reads as camelCase or kebab-case" $
        errorOf (compileTargetPattern "@/x/{{provider}}")
            `shouldBe` Just (AmbiguousCasing "provider" (CamelCase :| [KebabCase]))

    it "rejects a single-word name that reads as PascalCase or CONSTANT_CASE" $
        errorOf (compileTargetPattern "@/x/{{PROVIDER}}")
            `shouldBe` Just (AmbiguousCasing "PROVIDER" (PascalCase :| [ConstantCase]))

    it "accepts a single capitalised word, which is PascalCase only" $
        errorOf (compileTargetPattern "@/x/{{Provider}}") `shouldBe` Nothing

    it "rejects a name that is not written in any recognised casing" $ do
        errorOf (compileTargetPattern "@/x/{{Provider-Name}}") `shouldBe` Just (UnrecognisedCasing "Provider-Name")
        errorOf (compileTargetPattern "@/x/{{provider_name}}") `shouldBe` Just (UnrecognisedCasing "provider_name")

    it "rejects consecutive capitals, whose word boundaries are ambiguous" $ do
        errorOf (compileTargetPattern "@/x/{{HTTPClient}}") `shouldBe` Just (ConsecutiveCapitals "HTTPClient")
        errorOf (compileTargetPattern "@/x/{{httpAPIClient}}") `shouldBe` Just (ConsecutiveCapitals "httpAPIClient")

    it "accepts an acronym written as one word" $ do
        errorOf (compileTargetPattern "@/x/{{HttpClient}}") `shouldBe` Nothing
        errorOf (compileTargetPattern "@/x/{{http-client}}") `shouldBe` Nothing

    it "treats all four spellings of a name as one variable" $ do
        let bound spelling = (.boundVars) <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{HttpClient}}" `shouldBe` bound "{{httpClient}}"
        bound "{{HttpClient}}" `shouldBe` bound "{{http-client}}"
        bound "{{HttpClient}}" `shouldBe` bound "{{HTTP_CLIENT}}"

    it "treats all four spellings as one variable when a word carries a digit" $ do
        -- A CONSTANT_CASE segment carrying a digit is not all-uppercase by
        -- Data.Char, which used to split HTTP2_CLIENT letter by letter and
        -- make it a different variable from http2-client.
        let bound spelling = (.boundVars) <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{Http2Client}}" `shouldBe` bound "{{http2Client}}"
        bound "{{Http2Client}}" `shouldBe` bound "{{http2-client}}"
        bound "{{Http2Client}}" `shouldBe` bound "{{HTTP2_CLIENT}}"

    it "reserves TARGET_DIR under every casing of its name" $ do
        errorOf (compileClausePattern Narrow mempty "{{target-dir}}/x") `shouldBe` Just (ReservedTargetDir "target-dir")
        errorOf (compileClausePattern Narrow mempty "{{targetDir}}/x") `shouldBe` Just (ReservedTargetDir "targetDir")
        errorOf (compileClausePattern Narrow mempty "{{TargetDir}}/x") `shouldBe` Just (ReservedTargetDir "TargetDir")
        errorOf (compileClausePattern Narrow mempty "{{TARGET_DIR}}/x") `shouldBe` Nothing

    it "rejects TARGET_DIR in a target pattern, where it cannot be captured" $ do
        errorOf (compileTargetPattern "{{TARGET_DIR}}/x") `shouldBe` Just (TargetDirInTargetPattern "TARGET_DIR")
        errorOf (compileTargetPattern "{{target-dir}}/x") `shouldBe` Just (TargetDirInTargetPattern "target-dir")

    it "rejects any variable in an exclude pattern, which binds nothing" $ do
        errorOf (compileExcludePattern "@/x/{{FileName}}") `shouldBe` Just (VariableInExcludePattern "FileName")
        errorOf (compileExcludePattern "@/x/{{TARGET_DIR}}") `shouldBe` Just (VariableInExcludePattern "TARGET_DIR")
        errorOf (compileExcludePattern "@/x/**/*.spec") `shouldBe` Nothing

    it "rejects two adjacent variables in a target pattern" $
        errorOf (compileTargetPattern "@/x/{{FileName}}{{ServiceType}}")
            `shouldBe` Just (NoBoundaryBetween "FileName" "ServiceType")

    it "allows adjacent variables in a clause, where they are substituted" $
        errorOf (compileClausePattern Narrow (Set.fromList [VarName "file-name", VarName "service-type"]) "@/x/{{FileName}}{{ServiceType}}")
            `shouldBe` Nothing

    it "rejects a clause variable the target never captures" $
        errorOf (compileClausePattern Narrow fileName "{{TARGET_DIR}}/{{provider-name}}")
            `shouldBe` Just (UnboundVariable (VarName "provider-name") fileName)

    it "reports malformed patterns as a syntax error" $
        errorOf (compileTargetPattern "@/x/{{unclosed") `shouldSatisfy` isMalformed

    describe "rendered messages" $ do
        it "names the ambiguity and suggests both readings" $ do
            let message = renderError (compileTargetPattern "@/x/{{provider}}")
            message `shouldSatisfy` T.isInfixOf "camelCase and kebab-case"
            message `shouldSatisfy` T.isInfixOf "{{providerName}}"
            message `shouldSatisfy` T.isInfixOf "{{provider-name}}"

        it "lists the bound variables and suggests the nearest match" $ do
            let scope = Set.fromList [VarName "provider-name", VarName "file-name"]
            let message = renderError (compileClausePattern Narrow scope "{{TARGET_DIR}}/{{provider-nam}}")
            message `shouldSatisfy` T.isInfixOf "file-name, provider-name"
            message `shouldSatisfy` T.isInfixOf "Did you mean {{provider-name}}?"

        it "points at the only accepted spelling of TARGET_DIR" $
            renderError (compileClausePattern Narrow mempty "{{target-dir}}/x")
                `shouldSatisfy` T.isInfixOf "{{TARGET_DIR}}"

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

globPlusProps :: Spec
globPlusProps = describe "glob+ variable laws" $ do
    prop "a variable captured in one casing is matchable in every casing" $ do
        vars <- forAll genVars
        values <- forAll (traverse (const genValue) vars)
        let target = unsafeCompileTarget (segments [braced (spell casing name) | (name, casing) <- vars])
        let path = segments [spell casing value | ((_, casing), value) <- zip vars values]

        env <- maybe failure pure (matchTargetAt target path)
        for_ (zip vars values) $ \((name, _), value) ->
            for_ allCasings $ \casing -> do
                let clause = unsafeCompileClauseIn target.boundVars (segments [braced (spell casing name)])
                (clause, casing, matchClauseAt clause env (segments [spell casing value]))
                    === (clause, casing, True)

    prop "all four spellings of a name denote the same variable" $ do
        name <- forAll genName
        let boundBy casing = (unsafeCompileTarget (segments [braced (spell casing name)])).boundVars
        for_ allCasings $ \casing -> boundBy casing === boundBy PascalCase

    prop "compiling any {{token}} yields a pattern or a rendered error" $ do
        token <- forAll genToken
        let outcome = compileTargetPattern (segments [braced token])
        assert $ either (not . T.null . renderGlobPlusError) (const True) outcome

    prop "moduleFromGlob produces a path that matchClauseAt accepts" $ do
        vars <- forAll genVars
        values <- forAll (traverse (const genValue) vars)
        let target = unsafeCompileTarget (segments [braced (spell casing name) | (name, casing) <- vars])
        let path = segments [spell casing value | ((_, casing), value) <- zip vars values]
        clauseCasings <- forAll (traverse (const (Gen.element allCasings)) vars)
        let clause =
                unsafeCompileClauseIn target.boundVars . segments $
                    braced "TARGET_DIR"
                        : [braced (spell casing name) | ((name, _), casing) <- zip vars clauseCasings]

        env <- maybe failure pure (matchTargetAt target path)
        expanded <- maybe failure pure (moduleFromGlob env clause)
        matchClauseAt clause env expanded === True
        renderClausePattern env clause === expanded

    prop "a repeated variable matches only when its occurrences agree" $ do
        name <- forAll genName
        value <- forAll genValue
        other <- forAll genValue
        let target =
                unsafeCompileTarget . segments $
                    [braced (spell KebabCase name), braced (spell PascalCase name)]
        let pathFor a b = segments [spell KebabCase a, spell PascalCase b]

        matchTargetAt target (pathFor value value) /== Nothing
        when (other /= value) $
            matchTargetAt target (pathFor value other) === Nothing

--------------------------------------------------------------------------------
-- Model-based properties
--------------------------------------------------------------------------------

{- | Laws about names and polarity, over generated spellings.

The structural laws that used to live here - that a variable binds the segment
it was planted in, that a binding never spans a @\/@, that matching agrees with
a model - are now P0, P6 and P10 in "Deslop.GlobPlusPropSpec", stated against a
brute-force oracle rather than a second partial model.
-}
globPlusModelProps :: Spec
globPlusModelProps = modifyMaxSuccess (const 1000) . describe "glob+ pattern laws" $ do
    prop "whatever a requiring clause accepts, a forbidding one accepts too" $ do
        name <- forAll Oracle.genVarName
        value <- forAll Oracle.genValue
        targetCasing <- forAll (Gen.element allCasings)
        clauseCasing <- forAll (Gen.element allCasings)
        spelling <- forAll (Oracle.genRendering targetCasing value)
        candidate <- forAll (Oracle.genRendering clauseCasing value)

        let target = unsafeCompileTarget (segments [braced (spell targetCasing name)])
        let scope = target.boundVars
        let clauseOf polarity = unsafeCompileClauseAs polarity scope (segments [braced (spell clauseCasing name)])
        env <- maybe failure pure (matchTargetAt target (segments [spelling]))

        let path = segments [candidate]
        when (matchClauseAt (clauseOf Narrow) env path) $
            matchClauseAt (clauseOf Widen) env path === True

    prop "a name of three or more words behaves like any other" $ do
        name <- forAll Oracle.genLongVarName
        value <- forAll Oracle.genValue
        targetCasing <- forAll (Gen.element allCasings)
        clauseCasing <- forAll (Gen.element allCasings)
        spelling <- forAll (Oracle.genRendering targetCasing value)

        -- Affixed on both sides, the use{{UseCaseName}}ViewModel idiom.
        let target = unsafeCompileTarget ("@/probe/use" <> braced (spell targetCasing name) <> "ViewModel")
        let clause =
                unsafeCompileClauseIn target.boundVars $
                    "@/probe/with" <> braced (spell clauseCasing name) <> "Container"

        env <- maybe failure pure (matchTargetAt target ("@/probe/use" <> spelling <> "ViewModel"))
        expanded <- maybe failure pure (moduleFromGlob env clause)
        matchClauseAt clause env expanded === True

    prop "every word of a long name survives a round trip through all four casings" $ do
        name <- forAll Oracle.genLongVarName
        value <- forAll Oracle.genValue
        let target =
                unsafeCompileTarget . segments $
                    [braced (spell casing name) | casing <- allCasings]
        let path = segments [spell casing value | casing <- allCasings]

        env <- maybe failure pure (matchTargetAt target path)
        for_ allCasings $ \casing ->
            varOf (spell KebabCase name) casing env === Just (spell casing value)

    prop "a forbidding clause accepts every spelling of what it captured" $ do
        name <- forAll Oracle.genVarName
        value <- forAll Oracle.genValue
        clauseCasing <- forAll (Gen.element allCasings)
        spelling <- forAll (Oracle.genRendering KebabCase value)
        candidate <- forAll (Oracle.genRendering clauseCasing value)

        let target = unsafeCompileTarget (segments [braced (spell KebabCase name)])
        let clause = unsafeCompileClauseAs Widen target.boundVars (segments [braced (spell clauseCasing name)])
        env <- maybe failure pure (matchTargetAt target (segments [spelling]))

        matchClauseAt clause env (segments [candidate]) === True

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | 1-3 variables with distinct multi-word names, each in a random casing.
genVars :: Gen [([Text], Casing)]
genVars = do
    count <- Gen.int (Range.linear 1 3)
    forM (take count ["alpha", "beta", "gamma"]) $ \suffix -> do
        stem <- genWord
        casing <- Gen.element allCasings
        pure ([stem, suffix], casing)

-- | A variable name of 2-3 words, so that its casing is never ambiguous.
genName :: Gen [Text]
genName = Gen.list (Range.linear 2 3) genWord

-- | A captured value of 1-3 words; unlike a name, one word is fine.
genValue :: Gen [Text]
genValue = Gen.list (Range.linear 1 3) genWord

genWord :: Gen Text
genWord = Gen.text (Range.linear 2 6) Gen.lower

-- | Arbitrary variable-token content, valid or not.
genToken :: Gen Text
genToken = Gen.text (Range.linear 1 8) (Gen.element ['a', 'b', 'X', 'Y', '-', '_', '0', '9'])

-- Helpers

-- | Spells a name, independently of the production implementation.
spell :: Casing -> [Text] -> Text
spell PascalCase = T.concat . fmap capitalise
spell CamelCase = \case
    [] -> ""
    (head' : rest) -> head' <> T.concat (capitalise <$> rest)
spell KebabCase = T.intercalate "-"
spell ConstantCase = T.intercalate "_" . fmap T.toUpper

capitalise :: Text -> Text
capitalise t = T.toUpper (T.take 1 t) <> T.drop 1 t

-- | Each variable gets its own path segment, so no boundary is ambiguous.
segments :: [Text] -> Text
segments = ("@/probe/" <>) . T.intercalate "/"

braced :: Text -> Text
braced t = "{{" <> t <> "}}"

allCasings :: [Casing]
allCasings = [minBound .. maxBound]

{- | Compiled patterns hold a 'Regex', which has no Eq instance, so assertions
compare the error side only.
-}
errorOf :: Either GlobPlusError a -> Maybe GlobPlusError
errorOf = leftToMaybe

renderError :: Either GlobPlusError a -> Text
renderError = maybe "" renderGlobPlusError . errorOf

isMalformed :: Maybe GlobPlusError -> Bool
isMalformed (Just (MalformedPattern _ _)) = True
isMalformed _ = False

unsafeCompileTarget :: Text -> CompiledTargetPattern
unsafeCompileTarget t = case compileTargetPattern t of
    Right compiled -> compiled
    Left err -> error $ "Failed to compile target pattern: " <> renderGlobPlusError err

{- | Compiles a clause in the legacy single-variable scope. Every rule in the
original suite binds only @file-name@, so this keeps those cases untouched.
-}
unsafeCompileClause :: Text -> CompiledClausePattern
unsafeCompileClause = unsafeCompileClauseIn (Set.singleton (VarName "file-name"))

unsafeCompileClauseIn :: Set VarName -> Text -> CompiledClausePattern
unsafeCompileClauseIn = unsafeCompileClauseAs Narrow

unsafeCompileClauseAs :: Polarity -> Set VarName -> Text -> CompiledClausePattern
unsafeCompileClauseAs polarity bound t = case compileClausePattern polarity bound t of
    Right compiled -> compiled
    Left err -> error $ "Failed to compile clause pattern: " <> renderGlobPlusError err

-- | Builds a match environment the way production does: by matching a target.
envFor :: Text -> Text -> MatchEnv
envFor pat path =
    fromMaybe (error $ "target " <> pat <> " did not match " <> path) $
        matchTargetAt (unsafeCompileTarget pat) path

-- | An environment binding nothing, for the defensive unbound-variable paths.
sparseEnv :: MatchEnv
sparseEnv = MatchEnv {targetDir = "@/features/x", variables = Map.empty}

casingOf :: Casing -> MatchEnv -> Maybe Text
casingOf = varOf "file-name"

varOf :: Text -> Casing -> MatchEnv -> Maybe Text
varOf name casing env = casedAs casing <$> Map.lookup (VarName name) env.variables

--------------------------------------------------------------------------------
-- Path shims
--------------------------------------------------------------------------------

{- | The matchers take a path already split into segments, because production
splits each module id once and reuses it. These cases are about matching rather
than about that, so they hand over a whole path and let the shim split it.
-}
matchTargetAt :: CompiledTargetPattern -> Text -> Maybe MatchEnv
matchTargetAt target = matchTarget target . segmentsOf

matchClauseAt :: CompiledClausePattern -> MatchEnv -> Text -> Bool
matchClauseAt clause env = matchClause clause env . segmentsOf
