module TypeScript.ModuleResolverSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.Config (Pattern (..))
import TypeScript.ModuleResolver (Match (..), match)

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
