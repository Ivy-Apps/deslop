module Deslop.Frontend.PipelineSpec (spec) where

import Data.Map.Strict qualified as Map
import Deslop.Command (Command (..))
import Deslop.Module (Module (..), ModuleName (..))
import Deslop.Problem (ProblemId (..), problemId)
import Effects.CLI (LogStyle (..))
import Fixtures.Deslop.Frontend.Pipeline (Outcome (..), runLines)
import Fixtures.FileSystem.Path (inProject)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Frontend.Pipeline" $ do
    describe "reporting" $ do
        it "reports every finding, fixable or not, under every command" $ do
            for_ [CheckC, FixC, BaselineC] $ \command -> do
                outcome <- runLines command mempty [(inProject "a.lines", "TODO\nok\nFIXME")]

                sort (fmap problemId outcome.problems)
                    `shouldBe` [ProblemId "no-fixme#a.lines", ProblemId "no-todo#a.lines"]

        it "reports a baselined finding but leaves its code as written" $ do
            let baseline = baselineOf ["no-todo#a.lines"]

            outcome <- runLines FixC baseline [(inProject "a.lines", "TODO")]

            fmap problemId outcome.problems `shouldBe` [ProblemId "no-todo#a.lines"]
            outcome.written `shouldBe` []
            Map.lookup (inProject "a.lines") outcome.files `shouldBe` Just "TODO"

    describe "fixing" $ do
        it "writes back only the files whose bytes changed, and logs each as modified" $ do
            outcome <-
                runLines
                    FixC
                    mempty
                    [ (inProject "dirty.lines", "ok\nTODO")
                    , (inProject "clean.lines", "ok")
                    , (inProject "unfixable.lines", "FIXME")
                    ]

            outcome.written `shouldBe` [inProject "dirty.lines"]
            Map.lookup (inProject "dirty.lines") outcome.files `shouldBe` Just "ok\nDONE"
            outcome.transcript `shouldBe` [(Change, "  modified  dirty.lines")]

        it "repairs a file the Baseline does not cover, next to one it does" $ do
            let baseline = baselineOf ["no-todo#kept.lines"]

            outcome <- runLines FixC baseline [(inProject "kept.lines", "TODO"), (inProject "fixed.lines", "TODO")]

            outcome.written `shouldBe` [inProject "fixed.lines"]

        it "never writes when checking or baselining" $
            for_ [CheckC, BaselineC] $ \command -> do
                outcome <- runLines command mempty [(inProject "a.lines", "TODO")]

                (outcome.written, outcome.transcript) `shouldBe` ([], [])

    describe "unreadable files" $ do
        -- A module the frontend cannot read still exists, and an import of it
        -- must still resolve to it.
        it "lowers a file it cannot read alongside the rest" $ do
            outcome <-
                runLines
                    CheckC
                    mempty
                    [(inProject "unread.lines", "TODO\n\xff"), (inProject "fine.lines", "ok")]

            fmap moduleName outcome.modules `shouldBe` ["fine.lines", "unread.lines"]

        it "logs what lower says about it, by its path from the project root" $ do
            outcome <- runLines CheckC mempty [(inProject "src/deep/unread.lines", "\xff")]

            outcome.transcript `shouldBe` [(Warning, "WARNING: src/deep/unread.lines is not UTF-8")]

        it "neither reports nor writes a file it cannot read" $ do
            outcome <- runLines FixC mempty [(inProject "unread.lines", "TODO\n\xff")]

            (outcome.problems, outcome.written) `shouldBe` ([], [])

    describe "lowering" $ do
        it "lowers every parsed file, whatever the command" $
            for_ [CheckC, FixC, BaselineC] $ \command -> do
                outcome <- runLines command mempty [(inProject "a.lines", "ok"), (inProject "src/b.lines", "TODO")]

                sort (fmap moduleName outcome.modules) `shouldBe` ["a.lines", "src/b.lines"]

        it "walks no file the frontend's tooling generated, nor any that is not a source" $ do
            outcome <-
                runLines
                    CheckC
                    mempty
                    [ (inProject "a.lines", "ok")
                    , (inProject "generated/b.lines", "TODO")
                    , (inProject "notes.txt", "TODO")
                    ]

            (fmap moduleName outcome.modules, outcome.problems) `shouldBe` (["a.lines"], [])

        it "honours the project's .gitignore" $ do
            outcome <-
                runLines
                    CheckC
                    mempty
                    [(inProject ".gitignore", "ignored.lines"), (inProject "a.lines", "ok"), (inProject "ignored.lines", "TODO")]

            (fmap moduleName outcome.modules, outcome.problems) `shouldBe` (["a.lines"], [])

moduleName :: Module -> Text
moduleName m = (head m.names).text
