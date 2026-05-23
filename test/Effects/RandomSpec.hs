module Effects.RandomSpec (spec) where

import Effectful (Eff, IOE, runEff)
import Effects.Random (Random, rGenRandomInt, runRandom)
import Hedgehog ((===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog (PropertyT, assert, evalIO, forAll)
import TestUtils (prop)

spec :: Spec
spec = describe "Effects.Random" $ do
    describe "rGenRandomInt" $ do
        prop "generates a random int within bounds" $ do
            low <- forAll . Gen.int $ Range.linear (-100) 100
            high <- forAll . Gen.int $ Range.linear low 100
            res <- runRand $ rGenRandomInt (low, high)
            assert (res >= low && res <= high)

        prop "always returns 1 when bounds are (1,1)" $ do
            res <- runRand $ rGenRandomInt (1, 1)
            res === 1

runRand :: Eff '[Random, IOE] a -> PropertyT IO a
runRand = evalIO . runEff . runRandom
