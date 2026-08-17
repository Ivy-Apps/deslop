{- | The shape of a rulebook file, exactly as a user may have written it.

A DTO is /raw/: nothing here has been checked beyond being well-formed YAML, so
a 'GlobDto' may hold any text at all. Turning one into a "Deslop.Rulebook" is
"Deslop.Rulebook.Compiler"'s job, and it is the only thing that may do so.
-}
module Deslop.Rulebook.Dto (
    RulebookDto (..),
    RuleDto (..),
    GlobDto (..),
    ForbidsDto (..),
    AllowsDto (..),
    UsesDto (..),
    ExistsDto (..),
    parseRulebookYaml,
) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Yaml (decodeEither')
import Deslop.Rulebook (RuleId)

data RulebookDto = RulebookDto
    { id :: Text
    , name :: Text
    , description :: Text
    , rules :: [RuleDto]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data RuleDto = RuleDto
    { id :: RuleId
    , description :: Text
    , target :: GlobDto
    , exclude :: Maybe [GlobDto]
    , forbids :: Maybe [ForbidsDto]
    , allows :: Maybe [AllowsDto]
    , uses :: Maybe [UsesDto]
    , exists :: Maybe [ExistsDto]
    , example :: Maybe Text
    , fix :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data ForbidsDto = ForbidsImportDto
    { target :: GlobDto
    , transitive :: Maybe Bool
    }
    deriving stock (Show, Eq)

instance FromJSON ForbidsDto where
    parseJSON = withObject "ForbidsDto" $ \v ->
        ForbidsImportDto <$> v .: "import" <*> v .:? "transitive"

newtype AllowsDto = AllowsImportDto
    { target :: GlobDto
    }
    deriving stock (Show, Eq)

instance FromJSON AllowsDto where
    parseJSON = withObject "AllowsDto" $ \v ->
        AllowsImportDto <$> v .: "import"

data UsesDto = UsesImportDto
    { target :: GlobDto
    , transitive :: Maybe Bool
    }
    deriving stock (Show, Eq)

instance FromJSON UsesDto where
    parseJSON = withObject "UsesDto" $ \v ->
        UsesImportDto <$> v .: "import" <*> v .:? "transitive"

newtype ExistsDto = ExistsModuleDto
    { target :: GlobDto
    }
    deriving stock (Show, Eq)

instance FromJSON ExistsDto where
    parseJSON = withObject "ExistsDto" $ \v ->
        ExistsModuleDto <$> v .: "module"

-- | A Glob+ pattern as written. Unchecked: it may not compile.
newtype GlobDto = GlobDto Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

parseRulebookYaml :: ByteString -> Either Text RulebookDto
parseRulebookYaml = first show . decodeEither'
