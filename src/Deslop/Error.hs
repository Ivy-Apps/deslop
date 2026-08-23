{- | The ways a run can fail before it reaches a verdict.

Rendering one for a human is "UI.humanReadable".
-}
module Deslop.Error (DeslopError (..)) where

import System.OsPath (OsPath)

data DeslopError
    = TsConfigNotFoundError OsPath
    | TsConfigParseError Text
    | RulebookError Text
    | InvalidRuleConfig Text
    deriving (Show, Eq)
