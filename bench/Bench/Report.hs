-- | Renders a compared run for a human reading a terminal.
module Bench.Report (renderReport) where

import Bench.Compare (
    Factor (..),
    Group (..),
    GroupResult (..),
    Limits (..),
    Metric (..),
    Outcome (..),
    Regression (..),
    Row (..),
    Run (..),
    Scope (..),
    Shift (..),
    Thresholds (..),
    asPercent,
    limits,
    rowMeasurement,
    rowShift,
 )
import Bench.Fixtures (Fixture (..), commandName)
import Bench.Measurement (Bytes (..), Measurement (..), Seconds (..))
import Bench.Reference (Environment (..), RecordedCase (..), Reference (..), ReferenceState (..))
import Data.Text qualified as T
import Text.Printf (printf)

renderReport :: Environment -> ReferenceState -> Run -> Text
renderReport env state run =
    T.unlines $
        [heading env, ""]
            <> environmentWarning env state
            <> [columnHeader]
            <> concatMap renderGroup run.groups
            <> renderRetired run.retired
            <> [""]
            <> renderOutcome run.outcome

heading :: Environment -> Text
heading env =
    T.intercalate
        " · "
        [ "Deslop benchmark"
        , "ghc " <> env.ghc
        , env.os <> "/" <> env.arch
        , show env.capabilities <> " of " <> show env.processors <> " cores"
        ]

{- | Says so when the Reference was taken under conditions this run cannot
reproduce. Compared anyway - the numbers may still be informative, and the
person reading is the one who knows whether they are.
-}
environmentWarning :: Environment -> ReferenceState -> [Text]
environmentWarning _ Unrecorded = []
environmentWarning current (Recorded reference)
    | current == reference.environment = []
    | otherwise =
        [ "⚠️  The Reference was recorded under different conditions:"
        , "      reference: " <> heading reference.environment
        , "        current: " <> heading current
        , "    Differences of this kind move the numbers on their own."
        , ""
        ]

columnHeader :: Text
columnHeader = row "" "time" "" "Δ" "alloc" "Δ"

renderGroup :: GroupResult -> [Text]
renderGroup result =
    [groupLabel result.group]
        <> fmap renderEntry result.entries
        <> maybe [] (\t -> [renderRow "  total (all projects)" t]) result.total
        <> [renderAggregate result.aggregate]
        <> [""]
  where
    renderEntry entry = renderRow ("  " <> fixtureOf entry) entry
    fixtureOf (Referenced fixture _ _) = fixture.name
    fixtureOf (Unreferenced fixture _) = fixture.name

groupLabel :: Group -> Text
groupLabel (CommandGroup cmd) = commandName cmd
groupLabel DerivedTotal = "all (check + fix + baseline, derived)"

{- | One measured line. Takes its label rather than reading it off the row, so
that the summed row - which has no fixture to be named after - renders through
exactly the same code as the fixtures it sums.
-}
renderRow :: Text -> Row a -> Text
renderRow label entry =
    row
        label
        (millis m.time)
        ("± " <> millis m.timeStdDev)
        (maybe "new" (percent . (.time)) shift)
        (megabytes m.allocated)
        (maybe "new" (percent . (.alloc)) shift)
  where
    m = rowMeasurement entry
    shift = rowShift entry

renderAggregate :: Maybe Shift -> Text
renderAggregate Nothing = row "  geomean" "" "" "-" "" "-"
renderAggregate (Just shift) =
    row "  geomean" "" "" (percent shift.time) "" (percent shift.alloc)

renderRetired :: [RecordedCase] -> [Text]
renderRetired [] = []
renderRetired retired =
    ["The Reference holds cases this run did not measure:"]
        <> fmap describe retired
        <> ["    They are excluded from every geomean above."]
  where
    describe r = "    missing  " <> r.command <> "  " <> r.fixture

renderOutcome :: Outcome -> [Text]
renderOutcome Ungated =
    [ "No Reference recorded yet, so there is nothing to compare against."
    , ""
    , "    just update-benchmark"
    ]
renderOutcome Passed = ["✅ No regression.  " <> renderLimits]
renderOutcome (Regressed rs) =
    fmap renderRegression (toList rs)
        <> [ ""
           , renderLimits
           , ""
           , "If this change is expected, accept the new numbers:"
           , ""
           , "    just update-benchmark"
           ]

renderRegression :: Regression -> Text
renderRegression r =
    "❌ "
        <> commandName r.command
        <> " "
        <> metricName r.metric
        <> " "
        <> scopeName r.scope
        <> ": "
        <> T.strip (percent r.observed)
        <> " (limit "
        <> T.strip (percent r.limit)
        <> ")"
  where
    metricName TimeMetric = "time"
    metricName AllocMetric = "alloc"

    scopeName SuiteGeomean = "geomean"
    scopeName (SingleFixture f) = "on " <> f.name

renderLimits :: Text
renderLimits =
    "limits: time "
        <> thresholds limits.time
        <> " · alloc "
        <> thresholds limits.alloc
  where
    thresholds t =
        T.strip (percent t.geomean)
            <> " geomean / "
            <> T.strip (percent t.perFixture)
            <> " per fixture"

row :: Text -> Text -> Text -> Text -> Text -> Text -> Text
row name time stdDev timeDelta alloc allocDelta =
    T.stripEnd . fold $
        [ T.justifyLeft 36 ' ' name
        , T.justifyRight 10 ' ' time
        , T.justifyRight 9 ' ' stdDev
        , T.justifyRight 10 ' ' timeDelta
        , T.justifyRight 13 ' ' alloc
        , T.justifyRight 10 ' ' allocDelta
        ]

millis :: Seconds -> Text
millis (Seconds s) = format "%.1f ms" (s * 1000)

megabytes :: Bytes -> Text
megabytes (Bytes b) = format "%.1f MB" (b / (1024 * 1024))

percent :: Factor -> Text
percent = format "%+.1f%%" . asPercent

format :: String -> Double -> Text
format spec = toText . (printf spec :: Double -> String)
