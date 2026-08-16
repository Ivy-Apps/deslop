{- | Measures 'Deslop.doWork' over the fixture projects and judges the result
against @bench/reference.yaml@.

Local-only by design; nothing in CI runs this. See
@docs/adr/0008-benchmarks-are-local-only.md@.
-}
module Main (main) where

import Bench.Compare (Outcome (..), Run (..), compareRun)
import Bench.Fixtures (Case (..), Fixture (..), cases, commandName)
import Bench.Harness (caseParams, runCase)
import Bench.Measurement (Measurement, measure)
import Bench.Reference (
    Environment,
    Reference (..),
    currentEnvironment,
    loadReference,
    recordOf,
    referencePath,
    saveReference,
 )
import Bench.Report (renderReport)
import Criterion (benchmarkWith')
import Criterion.Main.Options (defaultConfig)
import Criterion.Types (Config (..), Verbosity (..), whnfAppIO)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import GHC.Stats (getRTSStatsEnabled)

{- | What this invocation is for.

@just benchmark@ judges; @just update-benchmark@ accepts. Both measure, and
both print the same comparison - accepting is more useful when you can see what
you are accepting.
-}
data Mode = Judge | Accept
    deriving stock (Show, Eq)

main :: IO ()
main = do
    mode <- modeFromArgs <$> getArgs
    requireRtsStats
    state <- loadReference >>= either abort pure
    environment <- currentEnvironment
    measured <- traverse measureCase cases
    let run = compareRun state measured
    putText $ renderReport environment state run
    case mode of
        Accept -> accept environment measured
        Judge -> exitFor run.outcome

modeFromArgs :: [String] -> Mode
modeFromArgs args
    | "--update" `elem` args = Accept
    | otherwise = Judge

{- | The memory half of every comparison comes from the RTS statistics, which
are silently absent without @-T@. Better to say so in a second than to spend
three minutes producing a report with a hole in it.
-}
requireRtsStats :: IO ()
requireRtsStats = do
    enabled <- getRTSStatsEnabled
    unless enabled . abort $
        "RTS statistics are disabled, so allocations cannot be measured.\n"
            <> "The benchmark must run with +RTS -T, which deslop.cabal sets via\n"
            <> "-with-rtsopts. Something has overridden it."

measureCase :: Case -> IO (Case, Measurement)
measureCase c = do
    TIO.hPutStrLn stderr $ "  measuring  " <> commandName c.command <> "  " <> c.fixture.name
    params <- caseParams c
    (c,) . measure <$> benchmarkWith' config (whnfAppIO runCase params)

{- | Criterion's own reporting is silenced: this tool prints one table at the
end rather than criterion's per-benchmark commentary.
-}
config :: Config
config = defaultConfig {verbosity = Quiet}

accept :: Environment -> [(Case, Measurement)] -> IO ()
accept environment measured = do
    recorded <- getCurrentTime
    saveReference
        Reference
            { environment = environment
            , recorded = recorded
            , cases = uncurry recordOf <$> measured
            }
    putTextLn $ "✅ Recorded " <> toText referencePath

{- | A missing Reference is not a failure - it is what a first run looks like,
and there is nothing for it to have regressed against.
-}
exitFor :: Outcome -> IO ()
exitFor = \case
    Ungated -> exitSuccess
    Passed -> exitSuccess
    Regressed _ -> exitFailure

abort :: Text -> IO a
abort msg = do
    TIO.hPutStrLn stderr $ "❌ " <> msg
    exitFailure
