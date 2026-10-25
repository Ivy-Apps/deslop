module Frontend.TypeScript.ModuleRoleSpec (spec) where

import Deslop.Module (Module (..), ModuleId (..), ModuleRole (..))
import Fixtures.Deslop.Module (mkModuleAt)
import Frontend.TypeScript.ModuleRole (moduleRole, withSubjects)
import Test.Hspec
import TestUtils (rp)

spec :: Spec
spec = describe "Frontend.TypeScript.ModuleRole" $ do
    it "calls a file named as a test, a story or an end-to-end spec a test" $ do
        fmap (moduleRole . rp) ["src/a.spec.ts", "src/a.test.tsx", "src/Button.stories.tsx", "src/login.e2e.ts", "src/login.cy.ts"]
            `shouldBe` replicate 5 (Test Nothing)

    it "calls a file under a test folder a test" $ do
        fmap (moduleRole . rp) ["src/__tests__/a.ts", "src/__mocks__/api.ts", "test/setup.ts", "tests/a.ts", "e2e/login.ts", "cypress/support.ts", "playwright/fixtures.ts"]
            `shouldBe` replicate 7 (Test Nothing)

    it "calls a file named or filed as a benchmark a benchmark" $ do
        fmap (moduleRole . rp) ["src/sort.bench.ts", "bench/sort.ts", "benchmark/a.ts", "benchmarks/a.ts"]
            `shouldBe` replicate 4 Benchmark

    it "calls a benchmark among the tests a benchmark" $ do
        moduleRole (rp "tests/sort.bench.ts") `shouldBe` Benchmark

    it "calls everything else source, however close its name comes" $ do
        fmap (moduleRole . rp) ["src/test.ts", "src/testing/a.ts", "src/spec.ts", "src/latest.ts", "src/bench.ts", "src/a.ts"]
            `shouldBe` replicate 6 Source

    describe "withSubjects" $ do
        it "names as a test's subject the file it is named after, beside it" $ do
            subjectsAmong ["src/a/b.ts", "src/a/b.test.ts", "src/a/b.spec.tsx"]
                `shouldBe` [("src/a/b.spec.tsx", Just "src/a/b.ts"), ("src/a/b.test.ts", Just "src/a/b.ts")]

        it "names the same path's file, from a test folder or a __tests__ folder" $ do
            subjectsAmong ["src/a/b.ts", "src/c.ts", "test/a/b.test.ts", "src/__tests__/c.spec.ts"]
                `shouldBe` [("src/__tests__/c.spec.ts", Just "src/c.ts"), ("test/a/b.test.ts", Just "src/a/b.ts")]

        it "names the file nearest the root when several match" $ do
            subjectsAmong ["packages/x/src/a/b.ts", "src/a/b.ts", "tests/a/b.test.ts"]
                `shouldBe` [("tests/a/b.test.ts", Just "src/a/b.ts")]

        it "names no subject for a test named after nothing, nor after a test" $ do
            subjectsAmong ["src/Button.tsx", "src/Button.stories.tsx", "test/setup.ts", "src/gone.test.ts", "tests/helper.ts", "tests/helper.test.ts"]
                `shouldBe` [("src/Button.stories.tsx", Nothing), ("src/gone.test.ts", Nothing), ("test/setup.ts", Nothing), ("tests/helper.test.ts", Nothing), ("tests/helper.ts", Nothing)]

        it "leaves sources and benchmarks as they are" $ do
            subjectsAmong ["src/sort.ts", "src/sort.bench.ts"] `shouldBe` []

-- | Each test among files at these paths, and the path of the file it tests.
subjectsAmong :: [Text] -> [(Text, Maybe Text)]
subjectsAmong paths =
    sortOn fst [(m.id.text, (.text) <$> subject) | m <- withSubjects (lowered <$> paths), Test subject <- [m.role]]
  where
    lowered path = (mkModuleAt path path []) {role = moduleRole (rp path)}
