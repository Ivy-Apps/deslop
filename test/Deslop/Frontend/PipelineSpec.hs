module Deslop.Frontend.PipelineSpec (spec) where

import Data.Map.Strict qualified as Map
import Deslop.Command (Command (..))
import Deslop.Module (Module (..), ModuleName (..))
import Deslop.Problem (ProblemId (..), problemId)
import Deslop.Problem.Baseline (emptyBaseline)
import Effects.CLI (LogStyle (..))
import Fixtures.Deslop.Frontend.Pipeline (Outcome (..), inLines, runLines)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Frontend.Pipeline" $ do
    describe "reporting" $ do
        it "reports every finding, fixable or not, under every command" $ do
            for_ [CheckC, FixC, BaselineC] $ \command -> do
                outcome <- runLines command emptyBaseline [(inLines "a.lines", "TODO\nok\nFIXME")]

                sort (fmap problemId outcome.problems)
                    `shouldBe` [ProblemId "no-fixme#a.lines", ProblemId "no-todo#a.lines"]

        it "reports a baselined finding but leaves its code as written" $ do
            let baseline = baselineOf ["no-todo#a.lines"]

            outcome <- runLines FixC baseline [(inLines "a.lines", "TODO")]

            fmap problemId outcome.problems `shouldBe` [ProblemId "no-todo#a.lines"]
            outcome.written `shouldBe` []
            Map.lookup (inLines "a.lines") outcome.files `shouldBe` Just "TODO"

    describe "fixing" $ do
        it "writes back only the files whose bytes changed, and logs each as modified" $ do
            outcome <-
                runLines
                    FixC
                    emptyBaseline
                    [ (inLines "dirty.lines", "ok\nTODO")
                    , (inLines "clean.lines", "ok")
                    , (inLines "unfixable.lines", "FIXME")
                    ]

            outcome.written `shouldBe` [inLines "dirty.lines"]
            Map.lookup (inLines "dirty.lines") outcome.files `shouldBe` Just "ok\nDONE"
            outcome.transcript `shouldBe` [(Change, "  modified  dirty.lines")]

        it "repairs a file the Baseline does not cover, next to one it does" $ do
            let baseline = baselineOf ["no-todo#kept.lines"]

            outcome <- runLines FixC baseline [(inLines "kept.lines", "TODO"), (inLines "fixed.lines", "TODO")]

            outcome.written `shouldBe` [inLines "fixed.lines"]

        it "never writes when checking or baselining" $
            for_ [CheckC, BaselineC] $ \command -> do
                outcome <- runLines command emptyBaseline [(inLines "a.lines", "TODO")]

                (outcome.written, outcome.transcript) `shouldBe` ([], [])

    describe "parse errors" $ do
        it "logs a file that does not parse and lowers the rest" $ do
            outcome <-
                runLines
                    CheckC
                    emptyBaseline
                    [(inLines "broken.lines", "!oops"), (inLines "fine.lines", "ok")]

            outcome.transcript `shouldBe` [(Error, "❌ Error: cannot parse broken.lines")]
            fmap moduleName outcome.modules `shouldBe` ["fine.lines"]

        it "neither reports nor writes a file that does not parse" $ do
            outcome <- runLines FixC emptyBaseline [(inLines "broken.lines", "TODO\n!oops")]

            (outcome.problems, outcome.written) `shouldBe` ([], [])

    describe "lowering" $ do
        it "lowers every parsed file, whatever the command" $
            for_ [CheckC, FixC, BaselineC] $ \command -> do
                outcome <- runLines command emptyBaseline [(inLines "a.lines", "ok"), (inLines "src/b.lines", "TODO")]

                sort (fmap moduleName outcome.modules) `shouldBe` ["a.lines", "src/b.lines"]

        it "walks no file the frontend's tooling generated, nor any that is not a source" $ do
            outcome <-
                runLines
                    CheckC
                    emptyBaseline
                    [ (inLines "a.lines", "ok")
                    , (inLines "generated/b.lines", "TODO")
                    , (inLines "notes.txt", "TODO")
                    ]

            (fmap moduleName outcome.modules, outcome.problems) `shouldBe` (["a.lines"], [])

        it "honours the project's .gitignore" $ do
            outcome <-
                runLines
                    CheckC
                    emptyBaseline
                    [(inLines ".gitignore", "ignored.lines"), (inLines "a.lines", "ok"), (inLines "ignored.lines", "TODO")]

            (fmap moduleName outcome.modules, outcome.problems) `shouldBe` (["a.lines"], [])

moduleName :: Module -> Text
moduleName m = (head m.names).text
