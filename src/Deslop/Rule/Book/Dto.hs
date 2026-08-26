{- | The shape of a rulebook file, exactly as a user may have written it.

A DTO is /raw/: nothing here has been checked beyond being well-formed YAML, so
a 'GlobDto' may hold any text at all. Turning one into a "Deslop.Rule.Book" is
"Deslop.Rule.Book.Compiler"'s job, and it is the only thing that may do so.
-}
module Deslop.Rule.Book.Dto (
    RulebookDto (..),
    RuleDto (..),
    GlobDto (..),
    ForbidsDto (..),
    AllowsDto (..),
    UsesDto (..),
    ExistsDto (..),
    parseRulebookYaml,
) where

import Data.Aeson (FromJSON (..), Key, Object, Options (..), Value, camelTo2, defaultOptions, genericParseJSON, withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Yaml (decodeEither')
import Deslop.Rule.Book (RuleId)

{- | A rulebook, parameterised over what one of its rules is.

The envelope is the same before and after desugaring - only a /rule/ has sugar
in it - so it is written once and the phase is read off the parameter:
@RulebookDto RuleDto@ is what an author wrote, @RulebookDto DesugaredRuleDto@ is
what the compiler is given. Desugaring the whole file is then the 'Functor'
instance, which is why there is one.
-}
data RulebookDto rule = RulebookDto
    { id :: !Text
    , name :: !Text
    , description :: !Text
    , rules :: ![rule]
    }
    deriving stock (Show, Eq, Generic, Functor)

instance (FromJSON rule) => FromJSON (RulebookDto rule) where
    parseJSON = genericParseJSON rulebookOptions

data RuleDto = RuleDto
    { id :: !RuleId
    , description :: !Text
    , target :: !GlobDto
    , exclude :: Maybe [GlobDto]
    , forbids :: Maybe [ForbidsDto]
    , allows :: Maybe [AllowsDto]
    , allowsOnly :: Maybe [AllowsDto]
    , uses :: Maybe [UsesDto]
    , exists :: Maybe [ExistsDto]
    , example :: Maybe Text
    , fix :: !Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON RuleDto where
    parseJSON = genericParseJSON rulebookOptions

data ForbidsDto = ForbidsImportDto
    { target :: !GlobDto
    , transitive :: Maybe Bool
    }
    deriving stock (Show, Eq)

instance FromJSON ForbidsDto where
    parseJSON = withExactObject "ForbidsDto" ["import", "transitive"] $ \v ->
        ForbidsImportDto <$> v .: "import" <*> v .:? "transitive"

newtype AllowsDto = AllowsImportDto
    { target :: GlobDto
    }
    deriving stock (Show, Eq)

instance FromJSON AllowsDto where
    parseJSON = withExactObject "AllowsDto" ["import"] $ \v ->
        AllowsImportDto <$> v .: "import"

data UsesDto = UsesImportDto
    { target :: GlobDto
    , transitive :: Maybe Bool
    }
    deriving stock (Show, Eq)

instance FromJSON UsesDto where
    parseJSON = withExactObject "UsesDto" ["import", "transitive"] $ \v ->
        UsesImportDto <$> v .: "import" <*> v .:? "transitive"

newtype ExistsDto = ExistsModuleDto
    { target :: GlobDto
    }
    deriving stock (Show, Eq)

instance FromJSON ExistsDto where
    parseJSON = withExactObject "ExistsDto" ["module"] $ \v ->
        ExistsModuleDto <$> v .: "module"

-- | A Glob+ pattern as written. Unchecked: it may not compile.
newtype GlobDto = GlobDto Text
    deriving stock (Show, Eq)
    deriving newtype (FromJSON)

{- | How every rulebook key is spelled, and what happens to one that is spelled
wrong.

A field of two or more words is one kebab-case key: @allowsOnly@ is written
@allows-only@. Every single-word key is left exactly as it was, so this changes
nothing about the rulebooks already in the wild - but it settles the spelling
for every multi-word key added after this one.

A key that is not one of those is an error rather than a shrug. Aeson's default
is to ignore what it does not recognise, which for a rulebook is the worst
possible answer: @allowsOnly@ instead of @allows-only@, or a @uses-optional@
that was never a feature, and the rule loads, passes, and enforces less than it
says it does. Nobody reads a clean run twice.
-}
rulebookOptions :: Options
rulebookOptions = defaultOptions {fieldLabelModifier = camelTo2 '-', rejectUnknownFields = True}

{- | 'withObject', for the instances written by hand rather than derived, and
strict about unknown keys in the same way 'rulebookOptions' makes the derived
ones strict. The permitted keys are listed because a hand-written parser has no
field names to read them off; keeping that list in step with the parser below
it is what "Deslop.Rule.Book.DtoSpec" is checking.
-}
withExactObject :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser a
withExactObject name permitted parse = withObject name $ \v ->
    case filter (`notElem` permitted) (KeyMap.keys v) of
        [] -> parse v
        unknown -> fail ("unknown fields: " <> show unknown)

parseRulebookYaml :: ByteString -> Either Text (RulebookDto RuleDto)
parseRulebookYaml = first show . decodeEither'
