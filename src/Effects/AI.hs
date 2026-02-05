module Effects.AI where

import Data.Aeson
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)
import Network.HTTP.Req

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
newtype GeminiApiKey = GeminiApiKey Text
data Gemini = Gemini
    { apiKey :: GeminiApiKey
    , model :: GeminiModel
    }

instance LLM Gemini where
    prompt :: Gemini -> Text -> IO (Either AIError Text)
    prompt llm p = pure $ Right ""

promptGemini :: Gemini -> Text -> IO (Either AIError Text)
promptGemini llm p = undefined
  where
    makeRequest :: IO ChatCompletionResponse
    makeRequest =
        runReq defaultHttpConfig $
            responseBody
                <$> req
                    POST
                    ( https "generativelanguage.googleapis.com"
                        /: "v1beta"
                        /: "models"
                        /: modelId
                        /: "generateContent"
                    )
                    (ReqBodyJson $ mkPayload)
                    jsonResponse
                    ("key" =: apiKey llm.apiKey)

    modelId = case llm.model of
        Flash2_5 -> "gemini-2.5-flash"
    apiKey (GeminiApiKey k) = k
    mkPayload =
        ChatCompletionRequest
            { contents = [GeminiChatMessage "user" [GeminiPart p]]
            , generationConfig = GenerationConfig 0.7
            }

data ChatCompletionRequest = ChatCompletionRequest
    { contents :: [GeminiChatMessage]
    , generationConfig :: GenerationConfig
    }
    deriving (Generic)
instance ToJSON ChatCompletionRequest

data GenerationConfig = GenerationConfig
    { temperature :: Double
    }
    deriving (Generic)
instance ToJSON GenerationConfig

data GeminiChatMessage = GeminiChatMessage
    { role :: Text
    , parts :: [GeminiPart]
    }
    deriving (Generic, Show)
instance ToJSON GeminiChatMessage
instance FromJSON GeminiChatMessage

data GeminiPart = GeminiPart
    { text :: Text
    }
    deriving (Generic, Show)
instance ToJSON GeminiPart
instance FromJSON GeminiPart

data ChatCompletionResponse = ChatCompletionResponse
    { candidates :: [Candidate]
    }
    deriving (Generic, Show)
instance FromJSON ChatCompletionResponse

data Candidate = Candidate
    { content :: GeminiChatMessage
    }
    deriving (Generic, Show)
instance FromJSON Candidate
