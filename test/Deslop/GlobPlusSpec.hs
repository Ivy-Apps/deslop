module Deslop.GlobPlusSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog (Gen, assert, failure, forAll, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec

import Deslop.Casing (render)
import Deslop.GlobPlus
import Deslop.GlobPlusModel qualified as Model
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import TestUtils (prop, requireJust)

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
            casingOf PascalCase env `shouldBe` Just "UserSettings"
            casingOf CamelCase env `shouldBe` Just "userSettings"
            casingOf KebabCase env `shouldBe` Just "user-settings"
            casingOf ConstantCase env `shouldBe` Just "USER_SETTINGS"

        it "rejects a path whose casing does not match the variable token" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            -- lowercase start violates {{FileName}} which requires [A-Z][a-zA-Z0-9]*
            matchTarget target "@/features/userSettingsView" `shouldBe` Nothing

        it "extracts {{fileName}} (camelCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{fileName}}Controller"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/userProfileController"

            casingOf CamelCase env `shouldBe` Just "userProfile"
            casingOf PascalCase env `shouldBe` Just "UserProfile"
            casingOf KebabCase env `shouldBe` Just "user-profile"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE"

        it "extracts {{file-name}} (kebab-case) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{file-name}}-repository"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/user-settings-repository"

            casingOf KebabCase env `shouldBe` Just "user-settings"
            casingOf PascalCase env `shouldBe` Just "UserSettings"
            casingOf CamelCase env `shouldBe` Just "userSettings"
            casingOf ConstantCase env `shouldBe` Just "USER_SETTINGS"

        it "extracts {{FileName}} preceded by a literal prefix (use{{FileName}}ViewModel)" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/auth/useUserAuthViewModel"

            env.targetDir `shouldBe` "@/features/auth"
            casingOf PascalCase env `shouldBe` Just "UserAuth"
            casingOf KebabCase env `shouldBe` Just "user-auth"

        it "does not match when the literal prefix differs from the pattern" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            matchTarget target "@/features/auth/getUserAuthViewModel" `shouldBe` Nothing

        it "extracts {{FileName}} surrounded by literal prefix and suffix (use{{FileName}}ViewModel.spec)" $ do
            let target = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel.spec"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/auth/useUserAuthViewModel.spec"

            env.targetDir `shouldBe` "@/features/auth"
            casingOf PascalCase env `shouldBe` Just "UserAuth"

        it "derives all casings correctly for a single-word name" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/HomeView"

            casingOf PascalCase env `shouldBe` Just "Home"
            casingOf CamelCase env `shouldBe` Just "home"
            casingOf KebabCase env `shouldBe` Just "home"
            casingOf ConstantCase env `shouldBe` Just "HOME"

        it "derives all casings correctly for a three-word compound name" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/admin/UserProfileSettingsContainer"

            env.targetDir `shouldBe` "@/features/admin"
            casingOf PascalCase env `shouldBe` Just "UserProfileSettings"
            casingOf CamelCase env `shouldBe` Just "userProfileSettings"
            casingOf KebabCase env `shouldBe` Just "user-profile-settings"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE_SETTINGS"

        it "derives TARGET_DIR correctly for deeply nested paths" $ do
            let target = unsafeCompileTarget "@/features/**/{{FileName}}Container"
            env <- requireJust "matchTarget returned Nothing" $ matchTarget target "@/features/auth/oauth/google/GoogleAuthContainer"

            env.targetDir `shouldBe` "@/features/auth/oauth/google"
            casingOf PascalCase env `shouldBe` Just "GoogleAuth"

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
            casingOf PascalCase envZero `shouldBe` Just "Home"
            envOne <-
                requireJust "one-subdir match failed" $
                    matchTarget target "@/features/auth/HomeContainer"
            casingOf PascalCase envOne `shouldBe` Just "Home"

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
            casingOf ConstantCase env `shouldBe` Just "MAX_RETRY_COUNT"
            casingOf PascalCase env `shouldBe` Just "MaxRetryCount"
            casingOf CamelCase env `shouldBe` Just "maxRetryCount"
            casingOf KebabCase env `shouldBe` Just "max-retry-count"

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
            casingOf PascalCase env `shouldBe` Just "HTTP"
            -- Derived casings treat "HTTP" as one word
            casingOf KebabCase env `shouldBe` Just "http"
            casingOf CamelCase env `shouldBe` Just "http"
            casingOf ConstantCase env `shouldBe` Just "HTTP"

        it "extracts {{FileName}} with an embedded digit (OAuth2Service)" $ do
            let target = unsafeCompileTarget "@/services/{{FileName}}Service"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/services/OAuth2Service"
            casingOf PascalCase env `shouldBe` Just "OAuth2"
            casingOf CamelCase env `shouldBe` Just "oAuth2"
            casingOf KebabCase env `shouldBe` Just "o-auth2"
            casingOf ConstantCase env `shouldBe` Just "O_AUTH2"

        it "extracts {{file-name}} combined with ** at zero subdirs" $ do
            let target = unsafeCompileTarget "@/features/**/{{file-name}}-service"
            envZero <-
                requireJust "zero-subdir failed" $
                    matchTarget target "@/features/user-auth-service"
            envZero.targetDir `shouldBe` "@/features"
            casingOf KebabCase envZero `shouldBe` Just "user-auth"
            casingOf PascalCase envZero `shouldBe` Just "UserAuth"
            envOne <-
                requireJust "one-subdir failed" $
                    matchTarget target "@/features/auth/user-auth-service"
            envOne.targetDir `shouldBe` "@/features/auth"
            casingOf KebabCase envOne `shouldBe` Just "user-auth"

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
            casingOf PascalCase env `shouldBe` Just "HomeView"

        -- TypeScript cross-casing: KebabCase ↔ PascalCase ↔ CamelCase
        it "derives all casings correctly for a three-word kebab-case name" $ do
            -- Typical TypeScript: file named user-profile-card.tsx → component UserProfileCard
            let target = unsafeCompileTarget "@/components/{{file-name}}"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/user-profile-card"
            casingOf KebabCase env `shouldBe` Just "user-profile-card"
            casingOf PascalCase env `shouldBe` Just "UserProfileCard"
            casingOf CamelCase env `shouldBe` Just "userProfileCard"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE_CARD"

        it "derives all casings correctly for a three-word camelCase name" $ do
            -- Typical TypeScript: service/hook named userProfileCardService
            let target = unsafeCompileTarget "@/services/{{fileName}}Service"
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/services/userProfileCardService"
            casingOf CamelCase env `shouldBe` Just "userProfileCard"
            casingOf PascalCase env `shouldBe` Just "UserProfileCard"
            casingOf KebabCase env `shouldBe` Just "user-profile-card"
            casingOf ConstantCase env `shouldBe` Just "USER_PROFILE_CARD"

    describe "matchClause" $ do
        let sampleEnv = envFor "@/features/user/{{FileName}}" "@/features/user/UserSettings"
        let richEnv = envFor "@/features/home/{{FileName}}" "@/features/home/HomeProfile"

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

        it "fails closed when a variable is absent from the environment" $ do
            -- Compilation rejects unbound variables, so this state is unreachable
            -- in practice. It must never widen a rule into matching everything.
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClause rule sparseEnv "@/features/x/AnythingView" `shouldBe` False
            matchClause rule sparseEnv "@/features/x/SomethingElseView" `shouldBe` False
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
            let env = envFor "src/v1.0/features/{{FileName}}" "src/v1.0/features/Home"
            let rule = unsafeCompileClause "{{TARGET_DIR}}/{{FileName}}View"
            matchClause rule env "src/v1.0/features/HomeView" `shouldBe` True
            matchClause rule env "src/v1X0/features/HomeView" `shouldBe` False

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
    let providerScope = boundVars providerTarget

    it "captures every variable in a target pattern" $ do
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget providerTarget "@/components/stripe-connect/payment/CheckoutView"

        env.targetDir `shouldBe` "@/components/stripe-connect/payment"
        varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
        varOf "service-type" KebabCase env `shouldBe` Just "payment"
        varOf "file-name" PascalCase env `shouldBe` Just "Checkout"

    it "enriches each variable independently into all four casings" $ do
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget providerTarget "@/components/stripe-connect/payment/CheckoutView"

        varOf "provider-name" PascalCase env `shouldBe` Just "StripeConnect"
        varOf "provider-name" CamelCase env `shouldBe` Just "stripeConnect"
        varOf "provider-name" ConstantCase env `shouldBe` Just "STRIPE_CONNECT"
        varOf "service-type" PascalCase env `shouldBe` Just "Payment"
        varOf "file-name" KebabCase env `shouldBe` Just "checkout"

    it "matches a clause that mixes several variables in different casings" $ do
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget providerTarget "@/components/stripe-connect/payment/CheckoutView"
        let clause = unsafeCompileClauseIn providerScope "@/services/{{provider-name}}/{{ServiceType}}{{FileName}}Client"

        matchClause clause env "@/services/stripe-connect/PaymentCheckoutClient" `shouldBe` True
        matchClause clause env "@/services/paypal/PaymentCheckoutClient" `shouldBe` False
        matchClause clause env "@/services/stripe-connect/PayoutCheckoutClient" `shouldBe` False

    it "expands several variables into a concrete module path" $ do
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget providerTarget "@/components/stripe-connect/payment/CheckoutView"
        let clause = unsafeCompileClauseIn providerScope "{{TARGET_DIR}}/{{provider-name}}-{{service-type}}-{{file-name}}"

        moduleFromGlob env clause
            `shouldBe` Just "@/components/stripe-connect/payment/stripe-connect-payment-checkout"

    it "renders unbound variables by name rather than as {{FileName}}" $ do
        let clause = unsafeCompileClauseIn providerScope "{{TARGET_DIR}}/{{provider-name}}/{{ServiceType}}"
        renderClausePattern sparseEnv clause
            `shouldBe` "@/features/x/{{provider-name}}/{{ServiceType}}"

    it "does not match when a variable's casing is violated" $ do
        matchTarget providerTarget "@/components/StripeConnect/payment/CheckoutView" `shouldBe` Nothing
        matchTarget providerTarget "@/components/stripe-connect/payment/checkoutView" `shouldBe` Nothing

    describe "a repeated variable" $ do
        let repeated = unsafeCompileTarget "@/components/{{provider-name}}/{{ProviderName}}View"

        it "binds once when every occurrence agrees" $ do
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget repeated "@/components/stripe-connect/StripeConnectView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
            varOf "provider-name" PascalCase env `shouldBe` Just "StripeConnect"
            boundVars repeated `shouldBe` Set.singleton (VarName "provider-name")

        it "does not match when the occurrences disagree" $ do
            matchTarget repeated "@/components/stripe-connect/PaypalView" `shouldBe` Nothing

        it "constrains two segments to the same value in one casing" $ do
            let sameTwice = unsafeCompileTarget "@/{{provider-name}}/{{provider-name}}-service"
            matchTarget sameTwice "@/stripe/stripe-service" `shouldSatisfy` isJust
            matchTarget sameTwice "@/stripe/paypal-service" `shouldBe` Nothing

    it "binds the leftmost variable greedily when a separator is consumable" $ do
        -- Both variables are kebab-case and '-' is a kebab character, so the
        -- boundary is ambiguous. POSIX longest-match settles it: documented,
        -- not accidental.
        let greedy = unsafeCompileTarget "@/x/{{provider-name}}-{{service-type}}"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget greedy "@/x/stripe-connect-payment-service"

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
        let bound spelling = boundVars <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{UseCaseName}}" `shouldBe` bound "{{useCaseName}}"
        bound "{{UseCaseName}}" `shouldBe` bound "{{use-case-name}}"
        bound "{{UseCaseName}}" `shouldBe` bound "{{USE_CASE_NAME}}"
        bound "{{UseCaseName}}" `shouldBe` Right (Set.singleton (VarName "use-case-name"))

    it "treats all four spellings of a five-word name as one variable" $ do
        let bound spelling = boundVars <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{UserProfileSettingsPageTitle}}" `shouldBe` bound "{{user-profile-settings-page-title}}"
        bound "{{UserProfileSettingsPageTitle}}" `shouldBe` bound "{{USER_PROFILE_SETTINGS_PAGE_TITLE}}"

    it "captures a three-word name between a prefix and a suffix" $ do
        let target = unsafeCompileTarget "@/application/use{{UseCaseName}}ViewModel"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/application/useArchiveOrderViewModel"

        varOf "use-case-name" PascalCase env `shouldBe` Just "ArchiveOrder"
        varOf "use-case-name" KebabCase env `shouldBe` Just "archive-order"
        varOf "use-case-name" CamelCase env `shouldBe` Just "archiveOrder"
        varOf "use-case-name" ConstantCase env `shouldBe` Just "ARCHIVE_ORDER"

    it "does not match when the suffix around the variable differs" $ do
        let target = unsafeCompileTarget "@/application/use{{UseCaseName}}ViewModel"
        matchTarget target "@/application/useArchiveOrderContainer" `shouldBe` Nothing
        matchTarget target "@/application/getArchiveOrderViewModel" `shouldBe` Nothing

    it "binds greedily when the captured value repeats the suffix" $ do
        let target = unsafeCompileTarget "@/application/use{{UseCaseName}}ViewModel"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/application/useArchiveViewModelViewModel"

        varOf "use-case-name" PascalCase env `shouldBe` Just "ArchiveViewModel"

    it "carries a three-word name across casings into a clause" $ do
        let target = unsafeCompileTarget "@/application/{{use-case-name}}/use{{UseCaseName}}ViewModel"
        let scope = boundVars target
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/application/archive-order/useArchiveOrderViewModel"
        let clause = unsafeCompileClauseIn scope "{{TARGET_DIR}}/{{UseCaseName}}UseCase"

        matchClause clause env "@/application/archive-order/ArchiveOrderUseCase" `shouldBe` True
        matchClause clause env "@/application/archive-order/ArchiveUseCase" `shouldBe` False
        moduleFromGlob env clause
            `shouldBe` Just "@/application/archive-order/ArchiveOrderUseCase"

    it "expands a three-word name into every casing at once" $ do
        let target = unsafeCompileTarget "@/application/{{UseCaseName}}UseCase"
        let scope = boundVars target
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/application/ArchiveOrderUseCase"
        let everyCasing = "@/x/{{UseCaseName}}/{{useCaseName}}/{{use-case-name}}/{{USE_CASE_NAME}}"
        let clause = unsafeCompileClauseIn scope everyCasing

        moduleFromGlob env clause
            `shouldBe` Just "@/x/ArchiveOrder/archiveOrder/archive-order/ARCHIVE_ORDER"

    it "agrees across casings when a three-word name carries an acronym" $ do
        let target = unsafeCompileTarget "@/x/{{use-case-name}}/{{UseCaseName}}UseCase"
        matchTarget target "@/x/archive-db-order/ArchiveDBOrderUseCase" `shouldSatisfy` isJust
        matchTarget target "@/x/http-client-pool/HTTPClientPoolUseCase" `shouldSatisfy` isJust
        matchTarget target "@/x/archive-db-order/ArchiveOrderUseCase" `shouldBe` Nothing

    it "repeats a three-word variable across a **/" $ do
        let target = unsafeCompileTarget "@/application/{{use-case-name}}/**/{{UseCaseName}}UseCase"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/application/archive-order/nested/ArchiveOrderUseCase"

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
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/stripe-connect/payment/CheckoutView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
            varOf "file-name" PascalCase env `shouldBe` Just "Checkout"

        it "keeps providers distinct rather than collapsing them onto the globstar" $ do
            paypal <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/paypal/payment/RefundView"
            stripe <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/stripe-connect/payment/CheckoutView"

            varOf "provider-name" KebabCase paypal `shouldNotBe` varOf "provider-name" KebabCase stripe

        it "binds the variable when the globstar matches zero directories" $ do
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/stripe-connect/CheckoutView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"

        it "never binds a variable to text containing a path separator" $ do
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/components/stripe-connect/payment/payout/CheckoutView"

            varOf "provider-name" KebabCase env `shouldNotSatisfy` any (T.isInfixOf "/")

    it "matches a repeated variable that straddles a **/" $ do
        let target = unsafeCompileTarget "@/components/{{provider-name}}/**/{{ProviderName}}View"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/components/stripe-connect/payment/StripeConnectView"

        varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
        varOf "provider-name" PascalCase env `shouldBe` Just "StripeConnect"

    describe "two separate **/ idioms" $ do
        let target = unsafeCompileTarget "@/{{provider-name}}/**/{{service-type}}/**/{{FileName}}View"

        it "binds every variable when only one parse is possible" $ do
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/stripe-connect/payment/CheckoutView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
            varOf "service-type" KebabCase env `shouldBe` Just "payment"
            varOf "file-name" PascalCase env `shouldBe` Just "Checkout"

        it "lets the leftmost ** bind greedily when several parses are possible" $ do
            -- 'a', 'payment' and 'b' are all valid kebab-case, so the boundary
            -- is genuinely ambiguous. POSIX longest-match settles it in favour
            -- of the leftmost **, exactly as it does for adjacent variables.
            env <-
                requireJust "matchTarget returned Nothing" $
                    matchTarget target "@/stripe-connect/a/payment/b/CheckoutView"

            varOf "provider-name" KebabCase env `shouldBe` Just "stripe-connect"
            varOf "service-type" KebabCase env `shouldBe` Just "b"
            varOf "file-name" PascalCase env `shouldBe` Just "Checkout"

    it "binds a variable that follows a trailing **" $ do
        -- A trailing ** is not the **/ idiom, so it introduces no extra group.
        -- This shape worked by accident before; it must keep working.
        let target = unsafeCompileTarget "@/components/{{provider-name}}/**"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget target "@/components/stripe-connect/payment/CheckoutView"

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
    let matchesFolder path = matchTarget repeated path `shouldSatisfy` isJust
    let rejectsFolder path = matchTarget repeated path `shouldBe` Nothing

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
        matchTarget constantTarget "@/x/HTTP2_CLIENT/Http2ClientView" `shouldSatisfy` isJust

    describe "expansion into a casing that was never captured" $ do
        let widget = unsafeCompileTarget "@/widgets/{{FileName}}Widget"
        let kebabOf path = do
                env <- requireJust "matchTarget returned Nothing" $ matchTarget widget path
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
    let scope = boundVars target
    let forbidding = unsafeCompileClauseAs Forbidding scope "@/internal/{{FileName}}/**"
    let requiring = unsafeCompileClauseAs Requiring scope "@/internal/{{FileName}}/**"
    let envOf = envFor "@/widgets/{{file-name}}"

    it "accepts every acronym spelling in a forbidding clause" $ do
        let env = envOf "@/widgets/db-connection"
        matchClause forbidding env "@/internal/DbConnection/x" `shouldBe` True
        matchClause forbidding env "@/internal/DBConnection/x" `shouldBe` True

    it "accepts only the canonical spelling in a requiring clause" $ do
        let env = envOf "@/widgets/db-connection"
        matchClause requiring env "@/internal/DbConnection/x" `shouldBe` True
        matchClause requiring env "@/internal/DBConnection/x" `shouldBe` False

    it "accepts every reading of an ambiguous capture in a forbidding clause" $ do
        -- ABTest could be ["ab","test"] or ["a","b","test"]; a forbidding
        -- clause must not let the reading it did not pick slip through.
        let env = envFor "@/widgets/{{FileName}}" "@/widgets/ABTest"
        let forbidsKebab = unsafeCompileClauseAs Forbidding (Set.singleton (VarName "file-name")) "@/internal/{{file-name}}/**"
        matchClause forbidsKebab env "@/internal/ab-test/x" `shouldBe` True
        matchClause forbidsKebab env "@/internal/a-b-test/x" `shouldBe` True

    it "does not widen a casing that has only one spelling" $ do
        let env = envOf "@/widgets/db-connection"
        let kebabClause polarity = unsafeCompileClauseAs polarity scope "@/internal/{{file-name}}/**"
        matchClause (kebabClause Forbidding) env "@/internal/db-connection/x" `shouldBe` True
        matchClause (kebabClause Forbidding) env "@/internal/dbConnection/x" `shouldBe` False

    it "keeps the literal capture exact under both polarities" $ do
        let pascalTarget = unsafeCompileTarget "@/widgets/{{FileName}}"
        let pascalScope = boundVars pascalTarget
        let clause polarity = unsafeCompileClauseAs polarity pascalScope "@/internal/{{FileName}}/**"
        env <-
            requireJust "matchTarget returned Nothing" $
                matchTarget pascalTarget "@/widgets/DBConnection"

        matchClause (clause Requiring) env "@/internal/DBConnection/x" `shouldBe` True
        matchClause (clause Forbidding) env "@/internal/DBConnection/x" `shouldBe` True

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
        let bound spelling = boundVars <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{HttpClient}}" `shouldBe` bound "{{httpClient}}"
        bound "{{HttpClient}}" `shouldBe` bound "{{http-client}}"
        bound "{{HttpClient}}" `shouldBe` bound "{{HTTP_CLIENT}}"

    it "treats all four spellings as one variable when a word carries a digit" $ do
        -- A CONSTANT_CASE segment carrying a digit is not all-uppercase by
        -- Data.Char, which used to split HTTP2_CLIENT letter by letter and
        -- make it a different variable from http2-client.
        let bound spelling = boundVars <$> compileTargetPattern ("@/x/" <> spelling)
        bound "{{Http2Client}}" `shouldBe` bound "{{http2Client}}"
        bound "{{Http2Client}}" `shouldBe` bound "{{http2-client}}"
        bound "{{Http2Client}}" `shouldBe` bound "{{HTTP2_CLIENT}}"

    it "reserves TARGET_DIR under every casing of its name" $ do
        errorOf (compileClausePattern Requiring mempty "{{target-dir}}/x") `shouldBe` Just (ReservedTargetDir "target-dir")
        errorOf (compileClausePattern Requiring mempty "{{targetDir}}/x") `shouldBe` Just (ReservedTargetDir "targetDir")
        errorOf (compileClausePattern Requiring mempty "{{TargetDir}}/x") `shouldBe` Just (ReservedTargetDir "TargetDir")
        errorOf (compileClausePattern Requiring mempty "{{TARGET_DIR}}/x") `shouldBe` Nothing

    it "rejects TARGET_DIR in a target pattern, where it cannot be captured" $ do
        errorOf (compileTargetPattern "{{TARGET_DIR}}/x") `shouldBe` Just (TargetDirInTargetPattern "TARGET_DIR")
        errorOf (compileTargetPattern "{{target-dir}}/x") `shouldBe` Just (TargetDirInTargetPattern "target-dir")

    it "rejects any variable in an exclude pattern, which binds nothing" $ do
        errorOf (compileExcludePattern "@/x/{{FileName}}") `shouldBe` Just (VariableInExcludePattern "FileName")
        errorOf (compileExcludePattern "@/x/{{TARGET_DIR}}") `shouldBe` Just (VariableInExcludePattern "TARGET_DIR")
        errorOf (compileExcludePattern "@/x/**/*.spec") `shouldBe` Nothing

    it "rejects two adjacent variables in a target pattern" $
        errorOf (compileTargetPattern "@/x/{{FileName}}{{ServiceType}}")
            `shouldBe` Just (AdjacentVariables "FileName" "ServiceType")

    it "allows adjacent variables in a clause, where they are substituted" $
        errorOf (compileClausePattern Requiring (Set.fromList [VarName "file-name", VarName "service-type"]) "@/x/{{FileName}}{{ServiceType}}")
            `shouldBe` Nothing

    it "rejects a clause variable the target never captures" $
        errorOf (compileClausePattern Requiring fileName "{{TARGET_DIR}}/{{provider-name}}")
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
            let message = renderError (compileClausePattern Requiring scope "{{TARGET_DIR}}/{{provider-nam}}")
            message `shouldSatisfy` T.isInfixOf "file-name, provider-name"
            message `shouldSatisfy` T.isInfixOf "Did you mean {{provider-name}}?"

        it "points at the only accepted spelling of TARGET_DIR" $
            renderError (compileClausePattern Requiring mempty "{{target-dir}}/x")
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

        env <- maybe failure pure (matchTarget target path)
        for_ (zip vars values) $ \((name, _), value) ->
            for_ allCasings $ \casing -> do
                let clause = unsafeCompileClauseIn (boundVars target) (segments [braced (spell casing name)])
                (clause, casing, matchClause clause env (segments [spell casing value]))
                    === (clause, casing, True)

    prop "all four spellings of a name denote the same variable" $ do
        name <- forAll genName
        let boundBy casing = boundVars (unsafeCompileTarget (segments [braced (spell casing name)]))
        for_ allCasings $ \casing -> boundBy casing === boundBy PascalCase

    prop "compiling any {{token}} yields a pattern or a rendered error" $ do
        token <- forAll genToken
        let outcome = compileTargetPattern (segments [braced token])
        assert $ either (not . T.null . renderGlobPlusError) (const True) outcome

    prop "moduleFromGlob produces a path that matchClause accepts" $ do
        vars <- forAll genVars
        values <- forAll (traverse (const genValue) vars)
        let target = unsafeCompileTarget (segments [braced (spell casing name) | (name, casing) <- vars])
        let path = segments [spell casing value | ((_, casing), value) <- zip vars values]
        clauseCasings <- forAll (traverse (const (Gen.element allCasings)) vars)
        let clause =
                unsafeCompileClauseIn (boundVars target) . segments $
                    braced "TARGET_DIR"
                        : [braced (spell casing name) | ((name, _), casing) <- zip vars clauseCasings]

        env <- maybe failure pure (matchTarget target path)
        expanded <- maybe failure pure (moduleFromGlob env clause)
        matchClause clause env expanded === True
        renderClausePattern env clause === expanded

    prop "a repeated variable matches only when its occurrences agree" $ do
        name <- forAll genName
        value <- forAll genValue
        other <- forAll genValue
        let target =
                unsafeCompileTarget . segments $
                    [braced (spell KebabCase name), braced (spell PascalCase name)]
        let pathFor a b = segments [spell KebabCase a, spell PascalCase b]

        matchTarget target (pathFor value value) /== Nothing
        when (other /= value) $
            matchTarget target (pathFor value other) === Nothing

--------------------------------------------------------------------------------
-- Model-based properties
--------------------------------------------------------------------------------

{- | These generate a pattern and a path together, from a model written
independently of the compiler, so a bug has to appear in both to go unnoticed.
-}
globPlusModelProps :: Spec
globPlusModelProps = modifyMaxSuccess (const 1000) . describe "glob+ pattern laws" $ do
    prop "binds every variable to the segment it was planted in" $ do
        slots <- forAll Model.genSlots
        planting <- forAll (Model.genPlanting slots)
        let target = unsafeCompileTarget (Model.renderPattern slots)

        env <- maybe failure pure (matchTarget target (Model.plantPath planting))
        for_ planting.segments $ \(slot, planted) -> case slot of
            Model.SlotVar name casing affix ->
                (name, varOf (render KebabCase name) casing env)
                    === (name, Model.unaffix affix =<< listToMaybe planted)
            _ -> pure ()

    prop "never binds a variable to text containing a path separator" $ do
        slots <- forAll Model.genSlots
        planting <- forAll (Model.genPlanting slots)
        let target = unsafeCompileTarget (Model.renderPattern slots)

        env <- maybe failure pure (matchTarget target (Model.plantPath planting))
        for_ (Map.elems env.variables) $ \bound ->
            for_ allCasings $ \casing ->
                assert (not (T.isInfixOf "/" (casedAs casing bound)))

    prop "matches exactly the paths the model says it should" $ do
        slots <- forAll Model.genSlots
        planting <- forAll (Model.genPlanting slots)
        pathSegments <- forAll (Model.genPerturbed planting)
        let target = unsafeCompileTarget (Model.renderPattern slots)

        isJust (matchTarget target (T.intercalate "/" pathSegments))
            === Model.matchesModel slots pathSegments

    prop "any two spellings of one name agree, however its acronyms are written" $ do
        name <- forAll Model.genName
        value <- forAll Model.genValue
        firstCasing <- forAll (Gen.element allCasings)
        secondCasing <- forAll (Gen.element allCasings)
        firstSpelling <- forAll (Model.genRendering firstCasing value)
        secondSpelling <- forAll (Model.genRendering secondCasing value)
        let target =
                unsafeCompileTarget . segments $
                    [braced (spell firstCasing name), braced (spell secondCasing name)]

        matchTarget target (segments [firstSpelling, secondSpelling]) /== Nothing

    prop "whatever a requiring clause accepts, a forbidding one accepts too" $ do
        name <- forAll Model.genName
        value <- forAll Model.genValue
        targetCasing <- forAll (Gen.element allCasings)
        clauseCasing <- forAll (Gen.element allCasings)
        spelling <- forAll (Model.genRendering targetCasing value)
        candidate <- forAll (Model.genRendering clauseCasing value)

        let target = unsafeCompileTarget (segments [braced (spell targetCasing name)])
        let scope = boundVars target
        let clauseOf polarity = unsafeCompileClauseAs polarity scope (segments [braced (spell clauseCasing name)])
        env <- maybe failure pure (matchTarget target (segments [spelling]))

        let path = segments [candidate]
        when (matchClause (clauseOf Requiring) env path) $
            matchClause (clauseOf Forbidding) env path === True

    prop "a name of three or more words behaves like any other" $ do
        name <- forAll Model.genLongName
        value <- forAll Model.genValue
        targetCasing <- forAll (Gen.element allCasings)
        clauseCasing <- forAll (Gen.element allCasings)
        spelling <- forAll (Model.genRendering targetCasing value)

        -- Affixed on both sides, the use{{UseCaseName}}ViewModel idiom.
        let target = unsafeCompileTarget ("@/probe/use" <> braced (spell targetCasing name) <> "ViewModel")
        let clause =
                unsafeCompileClauseIn (boundVars target) $
                    "@/probe/with" <> braced (spell clauseCasing name) <> "Container"

        env <- maybe failure pure (matchTarget target ("@/probe/use" <> spelling <> "ViewModel"))
        expanded <- maybe failure pure (moduleFromGlob env clause)
        matchClause clause env expanded === True

    prop "every word of a long name survives a round trip through all four casings" $ do
        name <- forAll Model.genLongName
        value <- forAll Model.genValue
        let target =
                unsafeCompileTarget . segments $
                    [braced (spell casing name) | casing <- allCasings]
        let path = segments [spell casing value | casing <- allCasings]

        env <- maybe failure pure (matchTarget target path)
        for_ allCasings $ \casing ->
            varOf (spell KebabCase name) casing env === Just (spell casing value)

    prop "a forbidding clause accepts every spelling of what it captured" $ do
        name <- forAll Model.genName
        value <- forAll Model.genValue
        clauseCasing <- forAll (Gen.element allCasings)
        spelling <- forAll (Model.genRendering KebabCase value)
        candidate <- forAll (Model.genRendering clauseCasing value)

        let target = unsafeCompileTarget (segments [braced (spell KebabCase name)])
        let clause = unsafeCompileClauseAs Forbidding (boundVars target) (segments [braced (spell clauseCasing name)])
        env <- maybe failure pure (matchTarget target (segments [spelling]))

        matchClause clause env (segments [candidate]) === True

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
unsafeCompileClauseIn = unsafeCompileClauseAs Requiring

unsafeCompileClauseAs :: Polarity -> Set VarName -> Text -> CompiledClausePattern
unsafeCompileClauseAs polarity bound t = case compileClausePattern polarity bound t of
    Right compiled -> compiled
    Left err -> error $ "Failed to compile clause pattern: " <> renderGlobPlusError err

-- | Builds a match environment the way production does: by matching a target.
envFor :: Text -> Text -> MatchEnv
envFor pat path =
    fromMaybe (error $ "target " <> pat <> " did not match " <> path) $
        matchTarget (unsafeCompileTarget pat) path

-- | An environment binding nothing, for the defensive unbound-variable paths.
sparseEnv :: MatchEnv
sparseEnv = MatchEnv {targetDir = "@/features/x", variables = Map.empty}

casingOf :: Casing -> MatchEnv -> Maybe Text
casingOf = varOf "file-name"

varOf :: Text -> Casing -> MatchEnv -> Maybe Text
varOf name casing env = casedAs casing <$> Map.lookup (VarName name) env.variables
