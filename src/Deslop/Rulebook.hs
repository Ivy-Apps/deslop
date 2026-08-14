{-# LANGUAGE QuasiQuotes #-}

module Deslop.Rulebook (
    RulebookDto (..),
    RuleDto (..),
    RuleId (..),
    GlobDto (..),
    ForbidsDto (..),
    AllowsDto (..),
    UsesDto (..),
    ExistsDto (..),
    Rulebook (..),
    Rule (..),
    ForbidsClause (..),
    AllowsClause (..),
    UsesClause (..),
    ExistsClause (..),
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
import Deslop.GlobPlus (CompiledClausePattern, CompiledExcludePattern, CompiledTargetPattern, GlobPlusError, Polarity (..), VarName, boundVars, compileClausePattern, compileExcludePattern, compileTargetPattern, renderGlobPlusError)
import Effectful
import Effects.FileSystem (AbsPath, RoFileSystem, fsDirectoryExists, fsListDirectory, fsReadFile, withAbsBaseUnsafe)
import System.OsPath (OsPath, osp)

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
    , exclude :: Maybe (NonEmpty CompiledExcludePattern)
    , executionContext :: ExecutionContext
    , forbids :: Maybe (NonEmpty ForbidsClause)
    , allows :: Maybe (NonEmpty AllowsClause)
    , uses :: Maybe (NonEmpty UsesClause)
    , exists :: Maybe (NonEmpty ExistsClause)
    , example :: Maybe Text
    , fix :: Text
    }
    deriving stock (Show)

data UsesClause = UsesImport
    { target :: CompiledClausePattern
    , transitive :: Bool
    }
    deriving stock (Show, Eq)

newtype FunctionName = FunctionName Text deriving (Show, Eq)

data ForbidsClause
    = ForbidsImport
        { target :: CompiledClausePattern
        , transitive :: Bool
        }
    | ForbidsFunctionCall
        { functionName :: FunctionName
        }
    deriving stock (Show, Eq)

newtype AllowsClause
    = AllowsImport
    { target :: CompiledClausePattern
    }
    deriving stock (Show, Eq)

newtype ExistsClause = ExistsModule
    { target :: CompiledClausePattern
    }
    deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

data RulebookDto = RulebookDto
    { id :: Text
    , name :: Text
    , description :: Text
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
    , description :: Text
    , target :: GlobDto
    , exclude :: Maybe [GlobDto]
    , executionContext :: Maybe ExecutionContextDto
    , forbids :: Maybe [ForbidsDto]
    , allows :: Maybe [AllowsDto]
    , uses :: Maybe [UsesDto]
    , exists :: Maybe [ExistsDto]
    , example :: Maybe Text
    , fix :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

newtype RuleId = RuleId Text
    deriving stock (Show, Eq, Ord)
    deriving newtype (FromJSON)

data ForbidsDto
    = ForbidsImportDto
        { target :: GlobDto
        , transitive :: Maybe Bool
        }
    | ForbidsFunctionCallDto
        { functionName :: Text
        }
    deriving stock (Show, Eq)

instance FromJSON ForbidsDto where
    parseJSON = withObject "ForbidsDto" $ \v ->
        (ForbidsImportDto <$> v .: "import" <*> v .:? "transitive")
            <|> (ForbidsFunctionCallDto <$> v .: "functional-call")

newtype AllowsDto
    = AllowsImportDto
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
    deriving (Show, Eq)

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

newtype GlobDto = GlobDto Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

--------------------------------------------------------------------------------
-- Loading
--------------------------------------------------------------------------------

rulesDir :: OsPath
rulesDir = [osp|deslop/rules|]

loadRuleBook :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text [Rulebook])
loadRuleBook projectPath = loadRuleBookFrom (withAbsBaseUnsafe projectPath rulesDir)

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
            , description = rbDto.description
            , rules = parsedRules
            }

{- | The target pattern is compiled first: the variables it captures are the
only ones its clauses may reference, so it defines the scope they compile in.
-}
ruleFromDto :: RuleDto -> Either Text Rule
ruleFromDto dto = do
    target <- compileGlob scope "target" compileTargetPattern dto.target
    let clauseScope = scope {bound = boundVars target}
    exclude <- compileGlobs scope "exclude" compileExcludePattern dto.exclude
    forbids <- compileForbidsClauses clauseScope dto.forbids
    allows <- compileAllowsClauses clauseScope dto.allows
    uses <- compileUsesClauses clauseScope dto.uses
    exists <- compileExistsClauses clauseScope dto.exists
    pure
        Rule
            { id = dto.id
            , description = dto.description
            , target = target
            , exclude = exclude
            , executionContext = mapExecutionContext dto.executionContext
            , forbids = forbids
            , allows = allows
            , uses = uses
            , exists = exists
            , example = dto.example
            , fix = dto.fix
            }
  where
    scope = RuleScope {ruleId = dto.id, bound = mempty}

compileForbidsClauses :: RuleScope -> Maybe [ForbidsDto] -> Either Text (Maybe (NonEmpty ForbidsClause))
compileForbidsClauses scope = traverseOptional compileForbids
  where
    compileForbids (ForbidsImportDto glob transitive) = do
        target <- compileClause Forbidding scope "forbids.import" glob
        Right ForbidsImport {target = target, transitive = fromMaybe False transitive}
    compileForbids (ForbidsFunctionCallDto name) =
        Right ForbidsFunctionCall {functionName = FunctionName name}

compileAllowsClauses :: RuleScope -> Maybe [AllowsDto] -> Either Text (Maybe (NonEmpty AllowsClause))
compileAllowsClauses scope = traverseOptional compileAllows
  where
    compileAllows (AllowsImportDto glob) =
        AllowsImport <$> compileClause Requiring scope "allows.import" glob

compileUsesClauses :: RuleScope -> Maybe [UsesDto] -> Either Text (Maybe (NonEmpty UsesClause))
compileUsesClauses scope = traverseOptional compileUses
  where
    compileUses (UsesImportDto glob transitive) = do
        target <- compileClause Requiring scope "uses.import" glob
        Right UsesImport {target = target, transitive = fromMaybe False transitive}

compileExistsClauses :: RuleScope -> Maybe [ExistsDto] -> Either Text (Maybe (NonEmpty ExistsClause))
compileExistsClauses scope = traverseOptional compileExists
  where
    compileExists (ExistsModuleDto glob) =
        ExistsModule <$> compileClause Requiring scope "exists.module" glob

-- | Compiles an optional list of DTOs, keeping it optional and non-empty.
traverseOptional :: (dto -> Either Text a) -> Maybe [dto] -> Either Text (Maybe (NonEmpty a))
traverseOptional _ Nothing = Right Nothing
traverseOptional compile (Just dtos) = nonEmpty <$> traverse compile dtos

--------------------------------------------------------------------------------
-- Glob+ compilation
--------------------------------------------------------------------------------

-- | Where a Glob+ pattern is being compiled, and what it may refer to.
data RuleScope = RuleScope
    { ruleId :: RuleId
    , bound :: Set VarName
    }

{- | Clauses compile against their polarity, which is fixed by the field they
came from rather than chosen at the call site.
-}
compileClause :: Polarity -> RuleScope -> Text -> GlobDto -> Either Text CompiledClausePattern
compileClause polarity scope field = compileGlob scope field (compileClausePattern polarity scope.bound)

compileGlobs ::
    RuleScope ->
    Text ->
    (Text -> Either GlobPlusError a) ->
    Maybe [GlobDto] ->
    Either Text (Maybe (NonEmpty a))
compileGlobs scope field compile = traverseOptional (compileGlob scope field compile)

{- | Compiles one pattern, labelling any failure with the rule and field it
came from so the author can find it in their rulebook.
-}
compileGlob :: RuleScope -> Text -> (Text -> Either GlobPlusError a) -> GlobDto -> Either Text a
compileGlob scope field compile (GlobDto glob) =
    first describe (compile glob)
  where
    RuleId rid = scope.ruleId
    describe err =
        "rule '"
            <> rid
            <> "', "
            <> field
            <> ": \""
            <> glob
            <> "\"\n"
            <> indent (renderGlobPlusError err)
    indent = T.intercalate "\n" . fmap ("  " <>) . T.lines

mapExecutionContext :: Maybe ExecutionContextDto -> ExecutionContext
mapExecutionContext Nothing = Neutral
mapExecutionContext (Just UseClientDto) = UseClient
mapExecutionContext (Just UseServerDto) = UseServer
