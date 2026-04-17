{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Deslop.Rulebook (
    RulebookDto (..),
    RuleDto (..),
    RuleId (..),
    GlobDto (..),
    ForbiddenDto (..),
    RuleBook (..),
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
import Effectful
import Effects.FileSystem (RoFileSystem, fsDirectoryExists, fsListDirectory, fsReadFile)
import System.FilePath.Glob qualified as Glob
import System.OsPath (OsPath, osp, (</>))

data Rulebook = Rulebook
    { name :: Text
    , description :: Text
    , rules :: [Rule]
    }
    deriving stock (Show, Eq)

data Rule = ForbiddenRule
    { id :: RuleId
    , description :: Maybe Text
    , target :: NonEmpty Glob.Pattern
    , exclude :: Maybe (NonEmpty Glob.Pattern)
    , forbidden :: [Forbidden]
    }
    deriving stock (Show, Eq)

data Forbidden = ForbiddenImport
    { target :: Glob.Pattern
    , transitive :: Bool
    }
    deriving stock (Show, Eq)

data RulebookDto = RulebookDto
    { name :: Text
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

loadRuleBook :: (RoFileSystem :> es) => Eff es (Either String (Maybe RuleBook))
loadRuleBook = loadRuleBookFrom rulesDir

loadRuleBookFrom :: (RoFileSystem :> es) => OsPath -> Eff es (Either String (Maybe RuleBook))
loadRuleBookFrom dir = fsDirectoryExists dir >>= bool (pure . Right $ Nothing) loadRules
  where
    loadRules =
        fsListDirectory dir
            >>= traverse (ruleBookFromFile . appendDir)
            >>= pure . fmap buildRuleBook . sequenceA

    appendDir p = dir </> p

    buildRuleBook [] = Nothing
    buildRuleBook xs = Just . mconcat . sortRuleBook $ xs
    sortRuleBook = sortOn (.name)

ruleBookFromFile :: (RoFileSystem :> es) => OsPath -> Eff es (Either String RuleBook)
ruleBookFromFile path =
    fsReadFile path
        >>= pure . fmap ruleBookFromDto . parseRuleBookYaml

parseRuleBookYaml :: ByteString -> Either String RulebookDto
parseRuleBookYaml = first show . decodeEither'

ruleBookFromDto :: RulebookDto -> RuleBook
ruleBookFromDto rbDto =
    RuleBook
        { name = rbDto.name
        , rules = mapMaybe ruleFromDto rbDto.rules
        }
  where
    ruleFromDto :: RuleDto -> Maybe Rule
    ruleFromDto (RuleDto rId desc target exclude (Just forbidden)) =
        Just $
            ForbiddenRule
                { id = rId
                , description = desc
                , target = compileGlobs target
                , exclude = compileGlobs <$> exclude
                , forbidden = forbiddenFromDto <$> forbidden
                }
    ruleFromDto _ = Nothing

    forbiddenFromDto :: ForbiddenDto -> Forbidden
    forbiddenFromDto (ForbiddenImportDto target transitive) =
        ForbiddenImport
            { target = compileGlob target
            , transitive = fromMaybe False transitive
            }

compileGlobs :: NonEmpty GlobDto -> NonEmpty Glob.Pattern
compileGlobs = fmap compileGlob

compileGlob :: GlobDto -> Glob.Pattern
compileGlob = Glob.compile . extractGlob
  where
    extractGlob :: GlobDto -> String
    extractGlob (GlobDto g) = g
