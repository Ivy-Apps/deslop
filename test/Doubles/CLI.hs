module Doubles.CLI (
    MockCLI (..),
    TestLogs (..),
    runMockCLI,
    defaultMockCLI,
    renderTranscript,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects.CLI (CLI (..), LogStyle, Stdout (..))

-- | Where, if anywhere, the messages a run logs are collected.
newtype MockCLI = MockCLI
    { logsRef :: Maybe (IORef TestLogs)
    }

defaultMockCLI :: MockCLI
defaultMockCLI = MockCLI {logsRef = Nothing}

-- | Every message the run logged, in the order it was logged.
newtype TestLogs = TestLogs
    { transcript :: [(LogStyle, Text)]
    }
    deriving (Show, Eq)

{- | Nothing Deslop runs reads from the terminal, so a 'ReadLine' is a test
that has wandered somewhere it should not. What a test logs is collected, never
shown on a terminal: its stdout is 'Redirected'.
-}
runMockCLI :: (IOE :> es) => MockCLI -> Eff (CLI : es) a -> Eff es a
runMockCLI mock = interpret $ \_ -> \case
    ReadLine -> error "runMockCLI: nothing under test reads a line"
    StdoutIs -> pure Redirected
    Log style msg -> case mock.logsRef of
        Nothing -> pure ()
        Just ref ->
            liftIO . modifyIORef' ref $
                TestLogs . (<> [(style, msg)]) . (.transcript)

{- | Renders a transcript for goldens, tagging each message with its style so a
regression in styling is as visible as one in wording.
-}
renderTranscript :: TestLogs -> Text
renderTranscript logs = T.unlines $ renderEntry <$> logs.transcript
  where
    renderEntry (style, msg) = "[" <> show style <> "] " <> msg
