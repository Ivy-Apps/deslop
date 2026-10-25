module Frontend.Haskell.ModuleRoleSpec (spec) where

import Data.Text qualified as T
import Deslop.Module (Module (..), ModuleRole (..))
import Fixtures.Frontend.Haskell.Project (lowerFiles, moduleNamed)
import Test.Hspec
import TestUtils (requireJust, requireRight)

cabal :: ByteString
cabal =
    encodeUtf8 . T.unlines $
        [ "cabal-version: 3.0"
        , "name: p"
        , "version: 0"
        , "library"
        , "  hs-source-dirs: src"
        , "test-suite p-test"
        , "  type: exitcode-stdio-1.0"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: test"
        , "benchmark p-bench"
        , "  type: exitcode-stdio-1.0"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: bench"
        ]

-- | The role of the module with this canonical name, among these files.
roleOf :: Text -> [(Text, ByteString)] -> IO ModuleRole
roleOf name files = do
    lowered <- requireRight show =<< lowerFiles (("p.cabal", cabal) : files)
    (.role) <$> requireJust (toString name) (moduleNamed name lowered)

spec :: Spec
spec = describe "Frontend.Haskell.ModuleRole" $ do
    it "calls a library module that imports no framework source" $ do
        roleOf "lib/p/P" [("src/P.hs", "module P where\nimport Data.Text\n")] `shouldReturn` Source

    it "calls every module of a test suite a test" $ do
        roleOf "test/p-test/Helpers" [("test/Helpers.hs", "module Helpers where\n")] `shouldReturn` Test

    it "calls every module of a benchmark a benchmark" $ do
        roleOf "bench/p-bench/Helpers" [("bench/Helpers.hs", "module Helpers where\n")] `shouldReturn` Benchmark

    it "calls a module importing a test framework a test, wherever it is" $ do
        forM_ ["Test.Hspec" :: ByteString, "Test.Hspec.QuickCheck", "Test.Tasty", "Test.HUnit"] $ \framework ->
            roleOf "lib/p/P" [("src/P.hs", "module P where\nimport " <> framework <> "\n")] `shouldReturn` Test

    it "calls a module importing a benchmark framework a benchmark, even under Test.Tasty" $ do
        forM_ ["Criterion.Main" :: ByteString, "Gauge", "Test.Tasty.Bench"] $ \framework ->
            roleOf "lib/p/P" [("src/P.hs", "module P where\nimport " <> framework <> "\n")] `shouldReturn` Benchmark

    it "calls a test suite's module importing a benchmark framework a benchmark" $ do
        roleOf "test/p-test/Perf" [("test/Perf.hs", "module Perf where\nimport Criterion\n")] `shouldReturn` Benchmark

    it "does not mistake a framework's namesake for it" $ do
        roleOf "lib/p/P" [("src/P.hs", "module P where\nimport Test.HspecLike\nimport Criteria\n")] `shouldReturn` Source
