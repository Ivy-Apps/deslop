{- | The ways a run can fail before it reaches a verdict.

Rendering one for a human is "UI.humanReadable".
-}
module Deslop.Error (DeslopError (..)) where

data DeslopError
    = -- | A language frontend could not make sense of the project: a
      -- @tsconfig.json@ that will not parse, a @go.mod@ that is not there.
      -- Already rendered, because the vocabulary is the frontend's and the
      -- core does not speak it.
      FrontendError Text
    | RulebookError Text
    | InvalidRuleConfig Text
    deriving (Show, Eq)
