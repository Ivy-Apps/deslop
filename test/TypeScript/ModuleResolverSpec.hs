module TypeScript.ModuleResolverSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.Config (Pattern (..))
import TypeScript.ModuleResolver (match)

spec :: Spec
spec = describe "ModuleResolver" $ do
    describe "match TSConfig pattern" $ do
        it "exact matching" $ do
            let pattern = Exact "react"
            -- Standard exact match
            match pattern "react" `shouldBe` True
            -- Fails on partial match / extended string
            match pattern "react-dom" `shouldBe` False
            -- Fails on case sensitivity difference
            match pattern "React" `shouldBe` False
            -- Fails on completely unrelated string
            match (Exact "Hi") "Hello" `shouldBe` False
            -- Fails on empty string
            match pattern "" `shouldBe` False

        it "suffix wildcard (something/*)" $ do
            let pattern = Wildcard "src/" ""
            -- Fails because the wildcard '*' requires at least one character
            match pattern "src/" `shouldBe` False
            -- Succeeds with a standard file
            match pattern "src/page.tsx" `shouldBe` True
            -- Succeeds with a deeply nested path
            match pattern "src/components/ui/button.tsx" `shouldBe` True
            -- Fails on entirely wrong prefix
            match pattern "test/util.ts" `shouldBe` False
            -- Fails on a partial/incomplete prefix match
            match pattern "sr/page.tsx" `shouldBe` False
            -- Fails on empty string
            match pattern "" `shouldBe` False

        it "infix wildcard (some/*-thing)" $ do
            let pattern = Wildcard "@types/" "-dto"
            -- Fails because the wildcard '*' requires at least one character
            match pattern "@types/-dto" `shouldBe` False
            -- Succeeds with standard single-word wildcard match
            match pattern "@types/a-dto" `shouldBe` True
            -- Succeeds with nested paths caught in the wildcard section
            match pattern "@types/user-reg/old/something-dto" `shouldBe` True
            -- Fails because the suffix has extra characters
            match pattern "@types/a-dtoS" `shouldBe` False
            -- Fails because of file extension in suffix when not expected
            match pattern "@types/a-dto.ts" `shouldBe` False
            -- Fails because the prefix does not match
            match pattern "@lib/a-dto" `shouldBe` False
            -- Fails on empty string
            match pattern "" `shouldBe` False

        it "prefix wildcard (*-something)" $ do
            let pattern = Wildcard "" "-spec.ts"
            -- Fails because the wildcard requires at least one character
            match pattern "-spec.ts" `shouldBe` False
            -- Succeeds because the wildcard matches a standard word
            match pattern "auth-spec.ts" `shouldBe` True
            -- Succeeds because the wildcard matches a nested directory path
            match pattern "src/components/button-spec.ts" `shouldBe` True
            -- Fails because the suffix does not match exactly (extra 'x')
            match pattern "auth-spec.tsx" `shouldBe` False
            -- Fails because it lacks the required suffix entirely
            match pattern "auth.ts" `shouldBe` False
            -- Fails on empty string
            match pattern "" `shouldBe` False
