module Doubles.CLI (
    MockCLI (..),
    TestLogs (..),
    runMockCLI,
    defaultMockCLI,
    renderTranscript,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Local qualified as State
import Effects.CLI (CLI (..), LogStyle)

data MockCLI = MockCLI
    { mockReadLines :: [Text]
    , logsRef :: Maybe (IORef TestLogs)
    }

defaultMockCLI :: MockCLI
defaultMockCLI =
    MockCLI
        { mockReadLines = []
        , logsRef = Nothing
        }

-- | Every message the run logged, in the order it was logged.
newtype TestLogs = TestLogs
    { transcript :: [(LogStyle, Text)]
    }
    deriving (Show, Eq)

runMockCLI :: (IOE :> es) => MockCLI -> Eff (CLI : es) a -> Eff es a
runMockCLI mock = reinterpret (State.evalState mock.mockReadLines) $ \_ -> \case
    ReadLine -> do
        remaining <- State.get @[Text]
        case remaining of
            [] ->
                error "runMockCLI: no more mocked readLine values were provided"
            (x : xs) -> do
                State.put @[Text] xs
                pure x
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
