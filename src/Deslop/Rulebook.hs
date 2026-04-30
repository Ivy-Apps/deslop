{-# LANGUAGE QuasiQuotes #-}

module Deslop.Rulebook (
    RulebookDto (..),
    RuleDto (..),
    RuleId (..),
    GlobDto (..),
    ForbiddenDto (..),
    Rulebook (..),
    Rule (..),
    Forbidden (..),
    RulebookId (..),
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
import Effects.FileSystem (AbsPath, RoFileSystem, fsDirectoryExists, fsListDirectory, fsMkAbsolute, fsReadFile)
import System.OsPath (OsPath, osp)
import Text.Megaparsec (errorBundlePretty)

newtype RulebookId = RulebookId Text deriving (Show, Eq, Ord)

data Rulebook = Rulebook
    { id :: RulebookId
    , name :: Text
    , description :: Text
    , rules :: [Rule]
    }
    deriving stock (Show)

data ExecutionContext = UseClient | UseServer | Neutral deriving (Show, Eq, Ord)

data Rule = Rule
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
    , fix :: Text
    }
    deriving stock (Show)

newtype FunctionName = FunctionName Text deriving (Show, Eq)

data Forbidden
    = ForbiddenImport
        { target :: CompiledRulePattern
        , transitive :: Bool
        }
    | ForbiddenFunctionCall
        { functionName :: FunctionName
        }
    deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

data RulebookDto = RulebookDto
    { id :: Text
    , name :: Text
    , description :: Maybe Text
    , rules :: [RuleDto]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

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
    , fix :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

newtype RuleId = RuleId Text
    deriving stock (Show, Eq, Ord)
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
loadRuleBookFrom dir = fsDirectoryExists dir >>= bool (pure . Right $ []) loadRules
  where
    loadRules =
        fsListDirectory dir
            >>= traverse ruleBookFromFile
            >>= pure . sequenceA

ruleBookFromFile :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text Rulebook)
ruleBookFromFile path =
    fsReadFile path
        >>= pure . (ruleBookFromDto <=< first T.pack . parseRuleBookYaml)

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
            { id = RulebookId rbDto.id
            , name = rbDto.name
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
        Rule
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
    pure ForbiddenFunctionCall {functionName = FunctionName name}
