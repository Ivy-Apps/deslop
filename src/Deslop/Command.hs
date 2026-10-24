{- | What a run was asked to do.

The driver owns it: a language frontend never learns whether its findings will
be fixed, reported or accepted into the Baseline.
-}
module Deslop.Command (Command (..)) where

data Command = CheckC | FixC | BaselineC | DiagramC
    deriving (Show, Eq)
