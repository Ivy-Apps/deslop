module Doubles.Random (
    runMockRandom,
) where

import Effectful
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Local qualified as State
import Effects.Random (Random (..))

{- | Interprets the Random effect using a list of pre-determined values.
Each call to rGenRandomInt consumes the next value from the list.
Crashes with a clear error if:
  - more random values are requested than were provided
  - a mocked value falls outside the requested bounds
-}
runMockRandom :: [Int] -> Eff (Random : es) a -> Eff es a
runMockRandom values = reinterpret (State.evalState values) $ \_ -> \case
    GenRandomInt (lo, hi) -> do
        remaining <- State.get @[Int]
        case remaining of
            [] ->
                error "runMockRandom: no more mocked random values were provided"
            (x : xs) -> do
                when (x < lo || x > hi) $
                    error $
                        "runMockRandom: mocked value "
                            <> show x
                            <> " is out of bounds ["
                            <> show lo
                            <> ", "
                            <> show hi
                            <> "]"
                State.put @[Int] xs
                pure x
