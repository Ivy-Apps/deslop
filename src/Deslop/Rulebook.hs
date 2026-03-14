{-# LANGUAGE TemplateHaskell #-}
module Deslop.RuleBook (
    RuleBookDto (..),
    RuleDto (..),
    RuleId (..),
    GlobDto (..),
    ForbiddenDto (..),
    RuleBook (..),
    Rule (..),
    Forbidden (..),
    nameL,
    rulesL,
    idL,
    descriptionL,
    targetL,
    excludeL,
    forbiddenL,
    parseRuleBookYaml,
    ruleBookFromDto,
) where

import Control.Lens.TH (makeLensesWith, lensRulesFor)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Char8 (ByteString)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import GHC.Generics (Generic)
import qualified System.FilePath.Glob as Glob

data RuleBookDto = RuleBookDto
    { name :: Text
    , rules :: [RuleDto]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON)

data RuleDto = RuleDto
    { id :: RuleId
    , description :: Maybe Text
    , target :: NonEmpty GlobDto
    , exclude :: Maybe (NonEmpty GlobDto)
    , forbidden :: Maybe [ForbiddenDto]
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

makeLensesWith (lensRulesFor [("name", "nameL"), ("rules", "rulesL")]) ''RuleBookDto
makeLensesWith
    (lensRulesFor
        [ ("id", "idL")
        , ("description", "descriptionL")
        , ("target", "targetL")
        , ("exclude", "excludeL")
        , ("forbidden", "forbiddenL")
        ]
    )
    ''RuleDto

data RuleBook = RuleBook
    { name :: Text
    , rules :: [Rule]
    }
    deriving stock (Show, Eq)

instance Semigroup RuleBook where
    rb1 <> rb2 =
        RuleBook
            { name = T.intercalate " <> " . sort . filter (not . T.null) $ [rb1.name, rb2.name]
            , rules = rb1.rules <> rb2.rules
            }

instance Monoid RuleBook where
    mempty =
        RuleBook
            { name = ""
            , rules = []
            }

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

parseRuleBookYaml :: ByteString -> Either String RuleBookDto
parseRuleBookYaml = first show . decodeEither'

ruleBookFromDto :: RuleBookDto -> RuleBook
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
