module Deslop.RuleBook (
    RuleBookDto (..),
    RuleDto (..),
    RuleId (..),
    RelativeModuleId (..),
    ForbiddenDto (..),
) where

import Data.Aeson (FromJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

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
    deriving anyclass (FromJSON)

newtype RelativeModuleId = RelativeModuleId Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)


-- parseRuleBookYaml :: ByteString -> Either 
