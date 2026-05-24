module Doubles.CLI (
    MockCLI (..),
    TestLogs (..),
    runMockCLI,
    defaultMockCLI,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Local qualified as State
import Effects.CLI (CLI (..))
import UI (problemsLogText)

data MockCLI = MockCLI
    { mockReadLines :: [Text]
    , problemsRef :: Maybe (IORef (Maybe TestLogs))
    }

defaultMockCLI :: MockCLI
defaultMockCLI =
    MockCLI
        { mockReadLines = []
        , problemsRef = Nothing
        }

newtype TestLogs = TestLogs
    { problems :: Text
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
    LogTitle _ -> pure ()
    LogModification _ -> pure ()
    LogFixSummary -> pure ()
    LogProblems ps -> case mock.problemsRef of
        Just pRef -> liftIO $ writeIORef pRef (Just . TestLogs . problemsLogText $ ps)
        Nothing -> pure ()
    LogNoProblemsFound -> pure ()
    LogBaselineSaved _ -> pure ()
    LogError _ -> pure ()
    LogText _ -> pure ()
    LogWarning _ -> pure ()
