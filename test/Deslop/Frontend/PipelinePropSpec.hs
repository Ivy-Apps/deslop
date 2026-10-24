{- | What the driver promises for every project: what it reports does not depend
on the Command, only @deslop fix@ writes, and a fix leaves nothing behind that
it was allowed to repair.
-}
module Deslop.Frontend.PipelinePropSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deslop.Command (Command (..))
import Deslop.Problem (isAutoFixable, problemId)
import Deslop.Problem.Baseline (Baseline, inBaseline)
import Effects.CLI (LogStyle (..))
import FileSystem.Path (AbsPath (..), decodeOsPath)
import Fixtures.Deslop.Frontend.Pipeline (Outcome (..), runLines)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Fixtures.FileSystem.Path (inProject)
import Hedgehog (Gen, assert, cover, evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.Frontend.Pipeline properties" $ do
    prop "checking and baselining never write" $ do
        project <- forAll genProject
        baseline <- forAll genBaseline
        for_ [CheckC, BaselineC] $ \command -> do
            outcome <- evalIO $ runLines command baseline project
            outcome.written === []
            outcome.files === Map.fromList project

    prop "what is reported does not depend on the command" $ do
        project <- forAll genProject
        baseline <- forAll genBaseline
        let reportedBy command =
                sort . fmap problemId . (.problems) <$> evalIO (runLines command baseline project)
        checked <- reportedBy CheckC
        fixed <- reportedBy FixC
        baselined <- reportedBy BaselineC
        cover 30 "something was reported" (not . null $ checked)
        (fixed, baselined) === (checked, checked)

    prop "after a fix, only what the Baseline accepted or nothing can repair is left" $ do
        project <- forAll genProject
        baseline <- forAll genBaseline
        fixed <- evalIO $ runLines FixC baseline project
        recheck <- evalIO . runLines CheckC baseline . Map.toList $ fixed.files
        cover 10 "a file was repaired" (not . null $ fixed.written)
        for_ recheck.problems $ \problem ->
            assert (inBaseline baseline problem || not (isAutoFixable problem))

    prop "a second fix writes nothing" $ do
        project <- forAll genProject
        baseline <- forAll genBaseline
        once <- evalIO $ runLines FixC baseline project
        twice <- evalIO . runLines FixC baseline . Map.toList $ once.files
        twice.written === []

    prop "fix logs exactly the files it wrote, in path order" $ do
        project <- forAll genProject
        baseline <- forAll genBaseline
        outcome <- evalIO $ runLines FixC baseline project
        [msg | (Change, msg) <- outcome.transcript] === (fmap modified . sort $ outcome.written)

modified :: AbsPath -> Text
modified = ("  modified  " <>) . fromMaybe "" . T.stripPrefix "/project/" . decodeOsPath . (.osPath)

-- | A flat Lines project: a few files, each a few statements.
genProject :: Gen [(AbsPath, ByteString)]
genProject = Gen.subsequence fileNames >>= traverse sourceOf
  where
    sourceOf name = (inProject name,) <$> Gen.frequency [(5, readable), (1, pure "TODO\n\xff")]
    readable = TE.encodeUtf8 . T.intercalate "\n" <$> Gen.list (Range.linear 1 5) genStatement
    genStatement = Gen.frequency [(3, pure "ok"), (3, pure "TODO"), (2, pure "FIXME")]

fileNames :: [Text]
fileNames = ["a.lines", "b.lines", "c.lines", "d.lines"]

-- | Some of the Problems a project could have, accepted.
genBaseline :: Gen Baseline
genBaseline = baselineOf <$> Gen.subsequence [rule <> "#" <> name | name <- fileNames, rule <- ["no-todo", "no-fixme"]]
