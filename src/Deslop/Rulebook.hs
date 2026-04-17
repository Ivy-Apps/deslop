{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Deslop.Rulebook (
    RulebookDto (..),
    RuleDto (..),
    RuleId (..),
    GlobDto (..),
    ForbiddenDto (..),
    Rulebook (..),
    Rule (..),
    Forbidden (..),
    parseRuleBookYaml,
    ruleBookFromDto,
    ruleBookFromFile,
    loadRuleBookFrom,
    loadRuleBook,
) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Yaml (decodeEither')
import Deslop.GlobPlus (CompiledRulePattern, CompiledTargetPattern)
import Effectful
import Effects.FileSystem (RoFileSystem, fsDirectoryExists, fsListDirectory, fsReadFile)
import System.OsPath (OsPath, osp, (</>))

data Rulebook = Rulebook
    { name :: Text
    , description :: Text
    , rules :: [Rule]
    }
    deriving stock (Show)

data Rule = ForbiddenRule
    { id :: RuleId
    , description :: Maybe Text
    , target :: CompiledTargetPattern
    , exclude :: Maybe (NonEmpty CompiledTargetPattern)
    , forbidden :: [Forbidden]
    }
    deriving stock (Show)

data Forbidden = ForbiddenImport
    { target :: CompiledRulePattern
    , transitive :: Bool
    }
    deriving stock (Show, Eq)

data RulebookDto = RulebookDto
    { name :: Text
    , description :: Text
    , rules :: [RuleDto]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data RuleDto = RuleDto
    { id :: RuleId
    , description :: Text
    , target :: GlobDto
    , exclude :: Maybe (NonEmpty GlobDto)
    , forbidden :: Maybe [ForbiddenDto]
    , example :: Maybe Text
    , fix :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

newtype RuleId = RuleId Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

data ForbiddenDto = ForbiddenImportDto
    { target :: GlobDto
    , transitive :: Maybe Bool
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON ForbiddenDto where
    parseJSON = withObject "ForbiddenDto" $ \v ->
        ForbiddenImportDto
            <$> v .: "import"
            <*> v .:? "transitive"

newtype GlobDto = GlobDto String
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

rulesDir :: OsPath
rulesDir = [osp|deslop/rules|]

loadRuleBook :: (RoFileSystem :> es) => Eff es (Either String [Rulebook])
loadRuleBook = loadRuleBookFrom rulesDir

loadRuleBookFrom :: (RoFileSystem :> es) => OsPath -> Eff es (Either String [Rulebook])
loadRuleBookFrom dir = fsDirectoryExists dir >>= bool (pure . Right $ []) loadRules
  where
    loadRules =
        fsListDirectory dir
            >>= traverse (ruleBookFromFile . appendDir)
            >>= pure . sequenceA

    appendDir p = dir </> p

ruleBookFromFile :: (RoFileSystem :> es) => OsPath -> Eff es (Either String Rulebook)
ruleBookFromFile path =
    fsReadFile path
        >>= pure . fmap ruleBookFromDto . parseRuleBookYaml

parseRuleBookYaml :: ByteString -> Either String RulebookDto
parseRuleBookYaml = first show . decodeEither'

ruleBookFromDto :: RulebookDto -> Rulebook
ruleBookFromDto rbDto =
    Rulebook
        { name = rbDto.name
        , description = rbDto.description
        , rules = mapMaybe ruleFromDto rbDto.rules
        }
  where
    ruleFromDto :: RuleDto -> Maybe Rule
    ruleFromDto _ = Nothing
