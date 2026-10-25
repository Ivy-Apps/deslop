module Frontend.TypeScript.ModuleRoleSpec (spec) where

import Deslop.Module (ModuleRole (..))
import Frontend.TypeScript.ModuleRole (moduleRole)
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Frontend.TypeScript.ModuleRole" $ do
    it "calls a file named as a test, a story or an end-to-end spec a test" $ do
        fmap (moduleRole . rp) ["src/a.spec.ts", "src/a.test.tsx", "src/Button.stories.tsx", "src/login.e2e.ts", "src/login.cy.ts"]
            `shouldBe` replicate 5 Test

    it "calls a file under a test folder a test" $ do
        fmap (moduleRole . rp) ["src/__tests__/a.ts", "src/__mocks__/api.ts", "test/setup.ts", "tests/a.ts", "e2e/login.ts", "cypress/support.ts", "playwright/fixtures.ts"]
            `shouldBe` replicate 7 Test

    it "calls a file named or filed as a benchmark a benchmark" $ do
        fmap (moduleRole . rp) ["src/sort.bench.ts", "bench/sort.ts", "benchmark/a.ts", "benchmarks/a.ts"]
            `shouldBe` replicate 4 Benchmark

    it "calls a benchmark among the tests a benchmark" $ do
        moduleRole (rp "tests/sort.bench.ts") `shouldBe` Benchmark

    it "calls everything else source, however close its name comes" $ do
        fmap (moduleRole . rp) ["src/test.ts", "src/testing/a.ts", "src/spec.ts", "src/latest.ts", "src/bench.ts", "src/a.ts"]
            `shouldBe` replicate 6 Source
