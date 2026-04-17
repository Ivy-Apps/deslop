module Deslop.GlobPlusSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Deslop.GlobPlus

-- | Helper to safely compile a target pattern in tests, failing the test suite immediately if parsing fails.
unsafeCompileTarget :: T.Text -> CompiledTargetPattern
unsafeCompileTarget t = case parseTargetPattern t of
    Right ast -> compileTargetPattern ast
    Left err -> error $ "Failed to parse target pattern: " ++ show err

-- | Helper to safely compile a rule pattern in tests.
unsafeCompileRule :: T.Text -> CompiledRulePattern
unsafeCompileRule t = case parseRulePattern t of
    Right ast -> compileRulePattern ast
    Left err -> error $ "Failed to parse rule pattern: " ++ show err

spec :: Spec
spec = do
    describe "Deslop.GlobPlus.matchTarget" $ do
        it "matches exact literal paths and derives TARGET_DIR" $ do
            let target = unsafeCompileTarget "src/app/page"
            let env = matchTarget target "src/app/page"
            fmap (\e -> e . targetDir) env `shouldBe` Just "src/app"

        it "returns Nothing when a literal path does not match" $ do
            let target = unsafeCompileTarget "src/app/page"
            matchTarget target "src/app/other" `shouldBe` Nothing

        it "matches wildcards (* and **) and derives the correct directory" $ do
            let target = unsafeCompileTarget "@/features/**/components/*"
            let env = matchTarget target "@/features/users/auth/components/Button"
            fmap (\e -> e . targetDir) env `shouldBe` Just "@/features/users/auth/components"

        it "extracts {{FileName}} (CamelCase) and enriches all other casings" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            let Just env = matchTarget target "@/features/UserSettingsView"

            env . targetDir `shouldBe` "@/features"
            Map.lookup CamelCase env . casings `shouldBe` Just "UserSettings"
            Map.lookup LowerCamelCase env . casings `shouldBe` Just "userSettings"
            Map.lookup KebabCase env . casings `shouldBe` Just "user-settings"
            Map.lookup ConstantCase env . casings `shouldBe` Just "USER_SETTINGS"

        it "strictly rejects a path if the variable casing does not match the token (e.g., lowerCamelCase passed to {{FileName}})" $ do
            let target = unsafeCompileTarget "@/features/{{FileName}}View"
            -- 'userSettings' starts with a lowercase letter, which violates {{FileName}} regex rules
            matchTarget target "@/features/userSettingsView" `shouldBe` Nothing

        it "extracts {{file-name}} (kebab-case) natively" $ do
            let target = unsafeCompileTarget "@/features/{{file-name}}-repository"
            let Just env = matchTarget target "@/features/user-settings-repository"

            -- Checks if the base capture works and the tokenization correctly formats the rest
            Map.lookup KebabCase env . casings `shouldBe` Just "user-settings"
            Map.lookup CamelCase env . casings `shouldBe` Just "UserSettings"
            Map.lookup ConstantCase env . casings `shouldBe` Just "USER_SETTINGS"

    describe "Deslop.GlobPlus.matchRule" $ do
        let sampleEnv =
                MatchEnv
                    { targetDir = "@/features/user"
                    , casings =
                        Map.fromList
                            [ (CamelCase, "UserSettings")
                            , (KebabCase, "user-settings")
                            ]
                    }

        it "interpolates {{TARGET_DIR}} and static strings successfully" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/data/repository"
            matchRule rule sampleEnv "@/features/user/data/repository" `shouldBe` True

        it "interpolates derived variable casings correctly (e.g., {{file-name}})" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/data/{{file-name}}-repository"
            matchRule rule sampleEnv "@/features/user/data/user-settings-repository" `shouldBe` True

        it "rejects paths where the interpolated variables are incorrect" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/data/{{file-name}}-repository"
            -- Mismatched directory
            matchRule rule sampleEnv "@/features/other/data/user-settings-repository" `shouldBe` False
            -- Mismatched casing (PascalCase instead of kebab-case)
            matchRule rule sampleEnv "@/features/user/data/UserSettings-repository" `shouldBe` False

        it "handles globs correctly alongside variables" $ do
            let rule = unsafeCompileRule "{{TARGET_DIR}}/**/*{{FileName}}*"
            matchRule rule sampleEnv "@/features/user/components/buttons/UserSettingsButton" `shouldBe` True
            matchRule rule sampleEnv "@/features/user/components/buttons/OtherButton" `shouldBe` False

    describe "End-to-End Scenarios" $ do
        it "validates the Page Architecture ViewModel rule end-to-end" $ do
            -- 1. Compile rules ahead of time
            let cTarget = unsafeCompileTarget "@/features/**/use{{FileName}}ViewModel"
            let cRule = unsafeCompileRule "{{TARGET_DIR}}/data/{{file-name}}-repository"

            -- 2. Hot-path matching (simulate file scanning)
            let targetPath = "@/features/auth/useUserAuthViewModel"
            let Just env = matchTarget cTarget targetPath

            -- 3. Assertions
            let validImport = "@/features/auth/data/user-auth-repository"
            let invalidImport = "@/features/auth/data/global-repository"
            let outsideImport = "@/features/other/data/user-auth-repository"

            matchRule cRule env validImport `shouldBe` True
            matchRule cRule env invalidImport `shouldBe` False
            matchRule cRule env outsideImport `shouldBe` False
