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

data AIError = IncorrectApiKey | GenericError Text deriving (Show, Eq)

data AI :: Effect where
    PromptLLM :: (LLM l) => l -> Text -> AI m (Either AIError Text)

type instance DispatchOf AI = Dynamic

promptLLM :: (AI :> es, LLM l) => l -> Text -> Eff es (Either AIError Text)
promptLLM = (send .) . PromptLLM

runAI :: (IOE :> es) => Eff (AI : es) a -> Eff es a
runAI = interpret $ \_ -> \case
    PromptLLM llm p -> liftIO $ prompt llm p

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
    prompt llm p =
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
