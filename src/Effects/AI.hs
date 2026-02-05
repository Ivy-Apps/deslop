module Effects.AI where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

data AIError = IncorrectApiKey | GenericError Text

data AI :: Effect where
    PromptLLM :: (LLM l) => l -> Text -> AI m (Either AIError Text)

type instance DispatchOf AI = Dynamic

promptLLM :: (AI :> es, LLM l) => l -> Text -> Eff es (Either AIError Text)
promptLLM = (send .) . PromptLLM

runAI :: (IOE :> es) => Eff (AI : es) a -> Eff es a
runAI = interpret $ \_ -> \case
    PromptLLM llm p -> pure . Left $ GenericError ("Not implemented: " <> p)

class LLM l where
    prompt :: l -> Text -> IO (Either AIError Text)

data GeminiModel = Flash2_5

data Gemini = Gemini
    { apiKey :: Text
    , model :: GeminiModel
    }

instance LLM Gemini where
    prompt :: Gemini -> Text -> IO (Either AIError Text)
    prompt llm p = pure $ Right ""
