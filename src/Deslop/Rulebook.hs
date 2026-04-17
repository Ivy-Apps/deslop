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

import Data.Aeson (FromJSON (..), withObject, withText, (.:), (.:?))
import Data.Text qualified as T
import Data.Yaml (decodeEither')
import Deslop.GlobPlus (CompiledRulePattern, CompiledTargetPattern, compileRulePattern, compileTargetPattern, parseRulePattern, parseTargetPattern)
import Effectful
import Effects.FileSystem (AbsPath, RoFileSystem, fsDirectoryExistsAbs, fsListAbsDirectory, fsMkAbsolute, fsReadAbsFile)
import System.OsPath (OsPath, osp)
import Text.Megaparsec (errorBundlePretty)

data Rulebook = Rulebook
    { name :: Text
    , description :: Text
    , rules :: [Rule]
    }
    deriving stock (Show)

data ExecutionContext = UseClient | UseServer | Neutral deriving (Show, Eq, Ord)

data Rule = ForbiddenRule
    { id :: RuleId
    , description :: Text
    , target :: CompiledTargetPattern
    , exclude :: Maybe (NonEmpty CompiledTargetPattern)
    , executionContext :: ExecutionContext
    , forbidden :: Maybe (NonEmpty Forbidden)
    , uses :: Maybe (NonEmpty CompiledRulePattern)
    , usesOptional :: Maybe (NonEmpty CompiledRulePattern)
    , exists :: Maybe (NonEmpty CompiledRulePattern)
    , example :: Maybe Text
    , fix :: Maybe Text
    }
    deriving stock (Show)

newtype FunctionName = FunctionName Text deriving (Show, Eq)

data Forbidden
    = ForbiddenImport
        { target :: CompiledRulePattern
        , transitive :: Bool
        }
    | FunctionCall
        { functionName :: FunctionName
        }
    deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

data RulebookDto = RulebookDto
    { name :: Text
    , description :: Maybe Text
    , rules :: [RuleDto]
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON RulebookDto where
    parseJSON = withObject "RulebookDto" $ \v ->
        RulebookDto
            <$> v .: "name"
            <*> v .:? "description"
            <*> v .: "rules"

data ExecutionContextDto = UseClientDto | UseServerDto deriving (Show, Eq)

instance FromJSON ExecutionContextDto where
    parseJSON = withText "ExecutionContextDto" $ \case
        "client" -> pure UseClientDto
        "server" -> pure UseServerDto
        other -> fail $ "Unknown execution-context: " <> T.unpack other

data RuleDto = RuleDto
    { id :: RuleId
    , description :: Maybe Text
    , target :: GlobDto
    , exclude :: Maybe [GlobDto]
    , executionContext :: Maybe ExecutionContextDto
    , forbidden :: Maybe [ForbiddenDto]
    , uses :: Maybe [GlobDto]
    , usesOptional :: Maybe [GlobDto]
    , exists :: Maybe [GlobDto]
    , example :: Maybe Text
    , fix :: Maybe Text
    }
    deriving stock (Show, Eq)

instance FromJSON RuleDto where
    parseJSON = withObject "RuleDto" $ \v ->
        RuleDto
            <$> v .: "id"
            <*> v .:? "description"
            <*> v .: "target"
            <*> v .:? "exclude"
            <*> v .:? "execution-context"
            <*> v .:? "forbidden"
            <*> v .:? "uses"
            <*> v .:? "uses-optional"
            <*> v .:? "exists"
            <*> v .:? "example"
            <*> v .:? "fix"

newtype RuleId = RuleId Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

data ForbiddenDto
    = ForbiddenImportDto
        { target :: GlobDto
        , transitive :: Maybe Bool
        }
    | FunctionCallDto
        { functionName :: Text
        }
    deriving stock (Show, Eq)

instance FromJSON ForbiddenDto where
    parseJSON = withObject "ForbiddenDto" $ \v ->
        (ForbiddenImportDto <$> v .: "import" <*> v .:? "transitive")
            <|> (FunctionCallDto <$> v .: "functional-call")

newtype GlobDto = GlobDto Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

--------------------------------------------------------------------------------
-- Loading
--------------------------------------------------------------------------------

rulesDir :: OsPath
rulesDir = [osp|deslop/rules|]

loadRuleBook :: (RoFileSystem :> es) => Eff es (Either Text [Rulebook])
loadRuleBook = fsMkAbsolute rulesDir >>= loadRuleBookFrom

loadRuleBookFrom :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text [Rulebook])
loadRuleBookFrom dir = fsDirectoryExistsAbs dir >>= bool (pure . Right $ []) loadRules
  where
    loadRules =
        fsListAbsDirectory dir
            >>= traverse ruleBookFromFile
            >>= pure . sequenceA

ruleBookFromFile :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text Rulebook)
ruleBookFromFile path =
    fsReadAbsFile path
        >>= pure . (>>= ruleBookFromDto) . first T.pack . parseRuleBookYaml

parseRuleBookYaml :: ByteString -> Either String RulebookDto
parseRuleBookYaml = first show . decodeEither'

--------------------------------------------------------------------------------
-- DTO → Domain
--------------------------------------------------------------------------------

ruleBookFromDto :: RulebookDto -> Either Text Rulebook
ruleBookFromDto rbDto = do
    parsedRules <- traverse ruleFromDto rbDto.rules
    pure
        Rulebook
            { name = rbDto.name
            , description = fromMaybe "" rbDto.description
            , rules = parsedRules
            }

ruleFromDto :: RuleDto -> Either Text Rule
ruleFromDto dto = do
    compiledTarget <- compileTargetGlob dto.target
    compiledExclude <- compileTargetGlobs dto.exclude
    compiledUses <- compileRuleGlobs dto.uses
    compiledUsesOptional <- compileRuleGlobs dto.usesOptional
    compiledExists <- compileRuleGlobs dto.exists
    compiledForbidden <- compileForbiddens dto.forbidden
    pure
        ForbiddenRule
            { id = dto.id
            , description = fromMaybe "" dto.description
            , target = compiledTarget
            , exclude = compiledExclude
            , executionContext = mapExecutionContext dto.executionContext
            , forbidden = compiledForbidden
            , uses = compiledUses
            , usesOptional = compiledUsesOptional
            , exists = compiledExists
            , example = dto.example
            , fix = dto.fix
            }

mapExecutionContext :: Maybe ExecutionContextDto -> ExecutionContext
mapExecutionContext Nothing = Neutral
mapExecutionContext (Just UseClientDto) = UseClient
mapExecutionContext (Just UseServerDto) = UseServer

compileTargetGlob :: GlobDto -> Either Text CompiledTargetPattern
compileTargetGlob (GlobDto s) =
    first (T.pack . show) (parseTargetPattern s)
        <&> compileTargetPattern

compileTargetGlobs :: Maybe [GlobDto] -> Either Text (Maybe (NonEmpty CompiledTargetPattern))
compileTargetGlobs Nothing = Right Nothing
compileTargetGlobs (Just globs) = fmap nonEmpty (traverse compileTargetGlob globs)

compileRuleGlob :: GlobDto -> Either Text CompiledRulePattern
compileRuleGlob (GlobDto s) =
    first (T.pack . show) (parseRulePattern s)
        <&> compileRulePattern

compileRuleGlobs :: Maybe [GlobDto] -> Either Text (Maybe (NonEmpty CompiledRulePattern))
compileRuleGlobs Nothing = Right Nothing
compileRuleGlobs (Just globs) = fmap nonEmpty (traverse compileRuleGlob globs)

compileForbiddens :: Maybe [ForbiddenDto] -> Either Text (Maybe (NonEmpty Forbidden))
compileForbiddens Nothing = Right Nothing
compileForbiddens (Just fbs) = fmap nonEmpty (traverse compileForbidden fbs)

compileForbidden :: ForbiddenDto -> Either Text Forbidden
compileForbidden (ForbiddenImportDto (GlobDto s) transitive) = do
    pattern <- first (T.pack . errorBundlePretty) (parseRulePattern s)
    pure ForbiddenImport {target = compileRulePattern pattern, transitive = fromMaybe False transitive}
compileForbidden (FunctionCallDto name) =
    pure FunctionCall {functionName = FunctionName name}
