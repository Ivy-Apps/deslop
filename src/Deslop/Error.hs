{- | The ways a run can fail before it reaches a verdict.

Rendering one for a human is "UI.humanReadable".
-}
module Deslop.Error (DeslopError (..)) where

data DeslopError
    = TsConfigError Text
    | RulebookError Text
    | InvalidRuleConfig Text
    deriving (Show, Eq)
