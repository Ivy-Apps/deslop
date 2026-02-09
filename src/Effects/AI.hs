module Effects.AI where

import Control.Exception (try)
import Control.Monad
import Control.Monad ((<=<))
import Data.Aeson
import Data.Bifunctor (first)
import Data.Either.Extra
import Data.Functor
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Utils
import Types
import Effectful.Reader.Static

data AIError = IncorrectApiKey | GenericError Text deriving (Show, Eq)
data LLMType = FastLLM deriving (Show, Eq)
data AnyLLM where
  AnyLLM :: (LLM l) => l -> AnyLLM

data AI :: Effect where
    PromptLLM :: LLMType -> Text -> AI m (Either AIError Text)

type instance DispatchOf AI = Dynamic

prompt :: (AI :> es) => LLMType -> Text -> Eff es (Either AIError Text)
prompt = (send .) . PromptLLM

runAI :: (IOE :> es) => Secrets -> Eff (AI : es) a -> Eff es a
runAI secrets = interpret $ \_ -> \case
    PromptLLM llmType p -> do 
      AnyLLM llm <- pure $ findLLM secrets llmType
      liftIO $ execPrompt llm p

findLLM :: Secrets -> LLMType -> AnyLLM
findLLM secrets FastLLM = 
  AnyLLM $ Gemini (GeminiApiKey secrets.geminiApiKey) Flash2_5

class LLM l where
    execPrompt :: l -> Text -> IO (Either AIError Text)

data GeminiModel = Flash2_5
newtype GeminiApiKey = GeminiApiKey Text
data Gemini = Gemini
    { apiKey :: GeminiApiKey
    , model :: GeminiModel
    }

instance LLM Gemini where
    execPrompt :: Gemini -> Text -> IO (Either AIError Text)
    execPrompt llm p =
        try @HttpException makeRequest
            <&> join . fmap extractText . first mapError
      where
        extractText :: ChatCompletionResponseDto -> Either AIError Text
        extractText =
            headErr (GenericError "No candidates") . (.candidates)
                >=> fmap (.text)
                    . headErr (GenericError "No parts in the message")
                    . (.parts)
                    . (.content)

        mapError :: HttpException -> AIError
        mapError = GenericError . T.pack . show

        makeRequest :: IO ChatCompletionResponseDto
        makeRequest =
            runReq defaultHttpConfig $
                responseBody
                    <$> req
                        POST
                        ( https "generativelanguage.googleapis.com"
                            /: "v1beta"
                            /: "models"
                            /: (modelId <> ":generateContent")
                        )
                        (ReqBodyJson mkPayload)
                        jsonResponse
                        ("key" =: apiKey llm.apiKey)

        modelId = case llm.model of
            Flash2_5 -> "gemini-2.5-flash"

        apiKey (GeminiApiKey k) = k

        mkPayload =
            ChatCompletionRequestDto
                { contents = [GeminiChatMessageDto "user" [GeminiPartDto p]]
                , generationConfig = GenerationConfigDto 0.0
                }

data ChatCompletionRequestDto = ChatCompletionRequestDto
    { contents :: [GeminiChatMessageDto]
    , generationConfig :: GenerationConfigDto
    }
    deriving (Generic, ToJSON)

newtype GenerationConfigDto = GenerationConfigDto
    { temperature :: Double
    }
    deriving (Generic, ToJSON)

newtype ChatCompletionResponseDto = ChatCompletionResponseDto
    { candidates :: [CandidateDto]
    }
    deriving (Generic, Show, FromJSON)

newtype CandidateDto = CandidateDto
    { content :: GeminiChatMessageDto
    }
    deriving (Generic, Show, FromJSON)

data GeminiChatMessageDto = GeminiChatMessageDto
    { role :: Text
    , parts :: [GeminiPartDto]
    }
    deriving (Generic, Show, ToJSON, FromJSON)

newtype GeminiPartDto = GeminiPartDto
    { text :: Text
    }
    deriving (Generic, Show, ToJSON, FromJSON)
