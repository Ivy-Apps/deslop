{- | Judges a run against the Reference.

Comparisons are made on ratios rather than on absolute times. The fixtures
differ in size by around forty times, so an arithmetic mean of their durations
is in practice the largest fixture's duration alone - a regression confined to
Glob+ matching, which only the small fixtures exercise, would not move it. Each
fixture's own new-over-old ratio, averaged geometrically, gives every fixture
equal say regardless of size.
-}
module Bench.Compare (
    Run (..),
    GroupResult (..),
    Group (..),
    Row (..),
    Entry,
    Total,
    rowMeasurement,
    rowShift,
    Shift (..),
    Outcome (..),
    Regression (..),
    Metric (..),
    Scope (..),
    Factor (..),
    Limits (..),
    Thresholds (..),
    limits,
    compareRun,
    asPercent,
) where

import Bench.Fixtures (Case (..), Fixture (..), commandName, fixtures)
import Bench.Measurement (Bytes (..), Measurement (..), Seconds (..), addMeasurement)
import Bench.Reference (RecordedCase (..), Reference (..), ReferenceState (..), lookupCase)
import Data.List.NonEmpty qualified as NE
import GHC.Float (log)
import Params (Command (..))

-- | A measurement over its Reference counterpart. @1.0@ is unchanged.
newtype Factor = Factor Double
    deriving stock (Show, Eq)
    deriving newtype (Ord)

-- | How time and allocations moved. Used per fixture and for the geomean.
data Shift = Shift
    { time :: Factor
    , alloc :: Factor
    }
    deriving stock (Show, Eq)

{- | One row of a group: what was measured now, and what the Reference holds
for it if it holds anything.

Something the Reference has never seen cannot be judged, and must be kept out
of the geomean rather than silently folded in at a ratio of 1.

The label is a parameter so that a summed row cannot be mistaken for a fixture
row - 'aggregateOf' only accepts @Row Fixture@, which is what stops the total
from being folded into the geomean of the things it is the total of.
-}
data Row a
    = -- | Measured now, and present in the Reference.
      Referenced a Measurement Measurement
    | -- | Measured now, absent from the Reference.
      Unreferenced a Measurement
    deriving stock (Show, Eq)

type Entry = Row Fixture

-- | Every fixture in a group added together. Carries no fixture of its own.
type Total = Row ()

rowMeasurement :: Row a -> Measurement
rowMeasurement (Referenced _ current _) = current
rowMeasurement (Unreferenced _ current) = current

rowShift :: Row a -> Maybe Shift
rowShift (Referenced _ current reference) = Just $ shiftOf current reference
rowShift (Unreferenced _ _) = Nothing

{- | What a block of the report covers.

'DerivedTotal' is reported but never gated: it is the sum of the three command
groups, so gating it would fail a second time for a regression already caught.
-}
data Group
    = CommandGroup Command
    | DerivedTotal
    deriving stock (Show, Eq)

data GroupResult = GroupResult
    { group :: Group
    , entries :: [Entry]
    , -- | Every project in the group added up: what one Deslop run over all of
      -- them costs in time and memory. Reported, never gated - it is the same
      -- measurements the entries above already account for.
      total :: Maybe Total
    , -- | Absent when no entry in the group had a Reference counterpart.
      aggregate :: Maybe Shift
    }
    deriving stock (Show, Eq)

data Outcome
    = -- | No Reference existed. There is nothing to judge.
      Ungated
    | Passed
    | Regressed (NonEmpty Regression)
    deriving stock (Show, Eq)

data Regression = Regression
    { command :: Command
    , metric :: Metric
    , scope :: Scope
    , observed :: Factor
    , limit :: Factor
    }
    deriving stock (Show, Eq)

data Metric = TimeMetric | AllocMetric
    deriving stock (Show, Eq)

data Scope
    = SuiteGeomean
    | SingleFixture Fixture
    deriving stock (Show, Eq)

data Thresholds = Thresholds
    { geomean :: Factor
    , perFixture :: Factor
    }
    deriving stock (Show, Eq)

data Limits = Limits
    { time :: Thresholds
    , alloc :: Thresholds
    }
    deriving stock (Show, Eq)

{- | What counts as a regression.

Time is allowed more room than allocations because it deserves less trust:
wall-clock measurement carries the machine's noise, whereas the same work
allocates the same bytes run after run. The pair also diagnoses rather than
just alarms - time up with allocations flat is a constant-factor or contention
problem, allocations up with time flat is a regression that got lucky with the
collector and will bite later.

These came from running the suite twice against identical code and reading the
spread. That pair of runs moved the time geomean by up to 2.9% and one fixture
by 9.3% - the small fixtures run in a millisecond or so, where a scheduling
hiccup is a large fraction of the total. Allocations moved by less than 0.05%
anywhere, which is why they are held twenty times tighter than the noise and
time is not.

Re-run the calibration and adjust here if the machine's noise floor moves.
-}
limits :: Limits
limits =
    Limits
        { time = Thresholds {geomean = Factor 1.05, perFixture = Factor 1.20}
        , alloc = Thresholds {geomean = Factor 1.01, perFixture = Factor 1.02}
        }

data Run = Run
    { groups :: [GroupResult]
    , -- | Cases the Reference holds that this run did not measure.
      retired :: [RecordedCase]
    , outcome :: Outcome
    }
    deriving stock (Show, Eq)

compareRun :: ReferenceState -> [(Case, Measurement)] -> Run
compareRun state measured =
    Run
        { groups = commandGroups <> [derivedTotal state measured]
        , retired = retiredCases state measured
        , outcome = verdict state commandGroups
        }
  where
    commandGroups = groupResult state measured <$> [CheckC, FixC, BaselineC]

verdict :: ReferenceState -> [GroupResult] -> Outcome
verdict Unrecorded _ = Ungated
verdict (Recorded _) groups =
    maybe Passed Regressed . nonEmpty . concatMap regressionsIn $ groups

{- | Every way one group broke its limits, over both metrics and both scopes.

Both scopes are checked because either alone lets something through: a geomean
absorbs one fixture blowing up among six that held steady, and a per-fixture
limit loose enough not to flap on noise cannot see a small drift across the
whole suite.
-}
regressionsIn :: GroupResult -> [Regression]
regressionsIn result = case result.group of
    DerivedTotal -> []
    CommandGroup cmd ->
        concatMap
            (uncurry (breaches cmd))
            [ (TimeMetric, limits.time)
            , (AllocMetric, limits.alloc)
            ]
  where
    breaches cmd metric thresholds =
        geomeanBreach cmd metric thresholds <> fixtureBreaches cmd metric thresholds

    geomeanBreach cmd metric thresholds =
        [ Regression cmd metric SuiteGeomean observed thresholds.geomean
        | Just shift <- [result.aggregate]
        , let observed = metricOf metric shift
        , observed > thresholds.geomean
        ]

    fixtureBreaches cmd metric thresholds =
        [ Regression cmd metric (SingleFixture fixture) observed thresholds.perFixture
        | entry@(Referenced fixture _ _) <- result.entries
        , Just shift <- [rowShift entry]
        , let observed = metricOf metric shift
        , observed > thresholds.perFixture
        ]

metricOf :: Metric -> Shift -> Factor
metricOf TimeMetric = (.time)
metricOf AllocMetric = (.alloc)

groupResult :: ReferenceState -> [(Case, Measurement)] -> Command -> GroupResult
groupResult state measured cmd =
    GroupResult
        { group = CommandGroup cmd
        , entries = entries
        , total = totalOf entries
        , aggregate = aggregateOf entries
        }
  where
    entries =
        [ entryFor state c m
        | (c, m) <- measured
        , c.command == cmd
        ]

{- | The @all@ row: each fixture's three commands summed, then compared.

Every fixture is always measured under all three commands - 'cases' is their
full cross product - but the Reference may predate one of them. A fixture is
therefore compared only when the Reference holds all three, so the total never
puts a sum of three against a sum of two.
-}
derivedTotal :: ReferenceState -> [(Case, Measurement)] -> GroupResult
derivedTotal state measured =
    GroupResult
        { group = DerivedTotal
        , entries = entries
        , total = totalOf entries
        , aggregate = aggregateOf entries
        }
  where
    entries = mapMaybe rowFor fixtures

    rowFor fixture = do
        current <- sumOver [m | (c, m) <- measured, c.fixture == fixture]
        pure $ case sumOver =<< referenced fixture of
            Just reference -> Referenced fixture current reference
            Nothing -> Unreferenced fixture current

    referenced fixture = case state of
        Unrecorded -> Nothing
        Recorded reference ->
            traverse (lookupCase reference . Case fixture) [CheckC, FixC, BaselineC]

entryFor :: ReferenceState -> Case -> Measurement -> Entry
entryFor state c current = case referenceFor state of
    Just reference -> Referenced c.fixture current reference
    Nothing -> Unreferenced c.fixture current
  where
    referenceFor Unrecorded = Nothing
    referenceFor (Recorded reference) = lookupCase reference c

{- | Adds every row in a group together, so the report can say what Deslopping
all of these projects costs end to end.

Compared against the Reference only when every row has a counterpart there -
a sum over six projects set against a sum over five would read as a large
saving that nobody made.
-}
totalOf :: [Entry] -> Maybe Total
totalOf entries = do
    current <- sumOver $ rowMeasurement <$> entries
    pure $ case sumOver =<< traverse referenceOf entries of
        Just reference -> Referenced () current reference
        Nothing -> Unreferenced () current
  where
    referenceOf (Referenced _ _ reference) = Just reference
    referenceOf (Unreferenced _ _) = Nothing

sumOver :: [Measurement] -> Maybe Measurement
sumOver = fmap (\(m :| ms) -> foldl' addMeasurement m ms) . nonEmpty

shiftOf :: Measurement -> Measurement -> Shift
shiftOf current reference =
    Shift
        { time = factorOf (secondsOf current.time) (secondsOf reference.time)
        , alloc = factorOf (bytesOf current.allocated) (bytesOf reference.allocated)
        }
  where
    secondsOf (Seconds s) = s
    bytesOf (Bytes b) = b

aggregateOf :: [Entry] -> Maybe Shift
aggregateOf entries = do
    shifts <- nonEmpty $ mapMaybe rowShift entries
    pure
        Shift
            { time = geometricMean $ (.time) <$> shifts
            , alloc = geometricMean $ (.alloc) <$> shifts
            }

{- | The geometric mean, which is the right average for ratios: a halving and a
doubling cancel to 1, where an arithmetic mean would call the pair a 25% loss.
-}
geometricMean :: NonEmpty Factor -> Factor
geometricMean fs =
    Factor . exp $ sum (log . unFactor <$> fs) / fromIntegral (NE.length fs)
  where
    unFactor (Factor f) = f

{- | Neither side is ever zero in practice - every case does real work - but a
zero would otherwise divide or take a logarithm of nothing further down. Such a
pair is reported as unchanged rather than as an infinite regression.
-}
factorOf :: Double -> Double -> Factor
factorOf current reference
    | reference <= 0 || current <= 0 = Factor 1
    | otherwise = Factor $ current / reference

retiredCases :: ReferenceState -> [(Case, Measurement)] -> [RecordedCase]
retiredCases Unrecorded _ = []
retiredCases (Recorded reference) measured =
    filter (not . wasMeasured) reference.cases
  where
    wasMeasured r =
        any
            (\(c, _) -> c.fixture.name == r.fixture && commandName c.command == r.command)
            measured

-- | A Factor as the signed percentage a reader expects: @Factor 1.05@ is @5.0@.
asPercent :: Factor -> Double
asPercent (Factor f) = (f - 1) * 100
