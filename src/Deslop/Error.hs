{- | The ways a run can fail before it reaches a verdict.

Rendering one for a human is "UI.humanReadable".
-}
module Deslop.Error (DeslopError (..), Warning (..)) where

import FileSystem.Path (ProjectRoot)

data DeslopError
    = {- | A language frontend could not make sense of the project: a
      @tsconfig.json@ that will not parse, a broken @go.mod@.
      Already rendered, because the vocabulary is the frontend's and the
      core does not speak it.
      -}
      FrontendError !Text
    | {- | No language frontend recognised the project: what was searched, and
      what each frontend looked for there, e.g. @TypeScript (tsconfig.json)@.
      -}
      NoFrontendDetected !ProjectRoot ![Text]
    | -- | @deslop diagram@ needs the d2 renderer, and it is not on @PATH@.
      D2NotFound
    | -- | d2 refused the diagram Deslop wrote; what it said on stderr.
      D2RenderFailed !Text
    | RulebookError !Text
    | InvalidRuleConfig !Text
    deriving (Show, Eq)

newtype Warning = Warning Text deriving (Show, Eq)
