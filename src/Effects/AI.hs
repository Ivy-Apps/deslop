module Effects.AI (
    AIError (..),
    LLMType (..),
    AI (..),
    prompt,
    runAI,
    LLM (..),
) where

import Control.Exception (try)
import Control.Monad ((<=<), (>=>))
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Secrets (GeminiApiKey (..), Secrets (..))
import Utils

data AIError
    = ApiKeyNotProvided
    | IncorrectApiKey
    | GenericError Text
    deriving (Show, Eq)
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
        case secrets.geminiApiKey of
            Just apiKey -> do
                AnyLLM llm <- pure $ findLLM apiKey llmType
                liftIO $ execPrompt llm p
            Nothing -> pure . Left $ ApiKeyNotProvided

findLLM :: GeminiApiKey -> LLMType -> AnyLLM
findLLM apiKey FastLLM =
    AnyLLM $ Gemini apiKey Flash2_5

class LLM l where
    execPrompt :: l -> Text -> IO (Either AIError Text)

data GeminiModel = Flash2_5
data Gemini = Gemini
    { apiKey :: GeminiApiKey
    , model :: GeminiModel
    }

instance LLM Gemini where
    execPrompt :: Gemini -> Text -> IO (Either AIError Text)
    execPrompt llm p =
        try @HttpException makeRequest
            >>= pure . (extractText <=< first mapError)
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
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype GenerationConfigDto = GenerationConfigDto
    { temperature :: Double
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype ChatCompletionResponseDto = ChatCompletionResponseDto
    { candidates :: [CandidateDto]
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

newtype CandidateDto = CandidateDto
    { content :: GeminiChatMessageDto
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data GeminiChatMessageDto = GeminiChatMessageDto
    { role :: Text
    , parts :: [GeminiPartDto]
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

newtype GeminiPartDto = GeminiPartDto
    { text :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)
