{- | What one measured case yields, and how it is read out of a criterion
'Report'.
-}
module Bench.Measurement (
    Measurement (..),
    Seconds (..),
    Bytes (..),
    measure,
    addMeasurement,
) where

import Criterion.Types (Measured (..), Report (..), SampleAnalysis (..), fromInt, rescale)
import Data.Aeson (FromJSON, ToJSON)
import Data.Vector qualified as V
import Statistics.Types (estPoint)

newtype Seconds = Seconds Double
    deriving stock (Show, Eq)
    deriving newtype (Ord, ToJSON, FromJSON)

newtype Bytes = Bytes Double
    deriving stock (Show, Eq)
    deriving newtype (Ord, ToJSON, FromJSON)

{- | One case's numbers, all per single run of 'Deslop.doWork'.

Peak memory is deliberately absent. The RTS reports it as a process-wide high
water mark, so within one benchmark process it only ever climbs - by the last
case it would describe every case that ran before it rather than that case.
-}
data Measurement = Measurement
    { time :: Seconds
    , timeStdDev :: Seconds
    , cpuTime :: Seconds
    , allocated :: Bytes
    , copied :: Bytes
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

{- | Adds two Measurements, which is what makes the derived @all@ total honest:
means are additive, and independent standard deviations combine in quadrature.

The total is very slightly pessimistic, because running the three commands back
to back would share warm caches that measuring them apart does not. That bias
is constant, so it cannot manufacture a regression.
-}
addMeasurement :: Measurement -> Measurement -> Measurement
addMeasurement a b =
    Measurement
        { time = onSeconds (+) a.time b.time
        , timeStdDev = onSeconds quadrature a.timeStdDev b.timeStdDev
        , cpuTime = onSeconds (+) a.cpuTime b.cpuTime
        , allocated = onBytes (+) a.allocated b.allocated
        , copied = onBytes (+) a.copied b.copied
        }
  where
    onSeconds f (Seconds x) (Seconds y) = Seconds (f x y)
    onBytes f (Bytes x) (Bytes y) = Bytes (f x y)
    quadrature x y = sqrt (x * x + y * y)

{- | Reads a Measurement out of criterion's analysis.

Wall time comes from the OLS regression, which is criterion's whole point - it
fits time against iteration count and so subtracts the measurement overhead.
The remaining figures have no such treatment available, so they are averaged
over the samples, each first rescaled to a single iteration.
-}
measure :: Report -> Measurement
measure report =
    Measurement
        { time = Seconds . estPoint $ analysis.anMean
        , timeStdDev = Seconds . estPoint $ analysis.anStdDev
        , cpuTime = Seconds . meanOver rescaled $ (.measCpuTime)
        , allocated = Bytes . meanOver raw $ perIteration (statOf measAllocated)
        , copied = Bytes . meanOver rescaled $ statOf measBytesCopied
        }
  where
    analysis = report.reportAnalysis
    raw = report.reportMeasured
    rescaled = V.map rescale raw

    meanOver :: V.Vector Measured -> (Measured -> Double) -> Double
    meanOver ms f = V.sum (V.map f ms) / fromIntegral (max 1 (V.length ms))

    {- 'rescale' normalises every field it touches to a single iteration, but it
    skips measAllocated - so that one is divided here, against the unrescaled
    samples. Averaging batch totals across batches of differing iteration counts
    would otherwise report a figure tens of times too large. -}
    perIteration :: (Measured -> Double) -> Measured -> Double
    perIteration f m = f m / fromIntegral (max 1 m.measIters)

    -- Unavailable GC statistics are unreachable: Main refuses to run at all
    -- unless the RTS was given -T.
    statOf :: (Measured -> Int64) -> Measured -> Double
    statOf f = maybe (error "Measurement: RTS stats missing despite -T") fromIntegral . fromInt . f
