module Deslop.RuleBook (
    RuleBookDto (..),
    RuleDto (..),
    RuleId (..),
    RelativeModuleId (..),
    ForbiddenDto (..),
    parseRuleBookYaml,
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.ByteString.Char8
import Data.Yaml (decodeEither')
import Data.Bifunctor (Bifunctor(first))

data RuleBookDto = RuleBookDto
    { name :: Text
    , rules :: [RuleDto]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data RuleDto = RuleDto
    { id :: RuleId
    , description :: Maybe Text
    , target :: NonEmpty RelativeModuleId
    , forbidden :: [ForbiddenDto]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

newtype RuleId = RuleId Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

data ForbiddenDto = ForbiddenImportDto
    { target :: RelativeModuleId
    , transitive :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON ForbiddenDto where
    parseJSON = withObject "ForbiddenDto" $ \v ->
        ForbiddenImportDto
            <$> (v .: "import" <|> v .: "target")
            <*> v .:? "transitive"

newtype RelativeModuleId = RelativeModuleId Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)


parseRuleBookYaml :: ByteString -> Either String RuleBookDto
parseRuleBookYaml = first show .  decodeEither'
