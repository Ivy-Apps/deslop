{- | The shape of a @tsconfig.json@ file, exactly as a user may have written it.

A DTO is /raw/: nothing here has been checked beyond being well-formed JSONC, so
an 'ExtendsDto' may name a file that does not exist and a 'CompilerOptionsDto'
may hold any text at all. Turning a chain of these into a "TypeScript.Config" is
"TypeScript.Config.Loader"'s job.

The file is JSON with comments - TypeScript accepts @\/\/@ and @\/* *\/@ in a
@tsconfig.json@ and every real-world config uses them - so the bytes are
stripped of comments before they are decoded.
-}
module TypeScript.Config.Dto (
    TsConfigDto (..),
    CompilerOptionsDto (..),
    ExtendsDto (..),
    parseTsConfigJson,
    stripTsComments,
) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecode')
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char (char)

data TsConfigDto = TsConfigDto
    { extends :: !(Maybe ExtendsDto)
    , compilerOptions :: !(Maybe CompilerOptionsDto)
    }
    deriving (Show, Eq, Generic)

{- | What a config's @extends@ names, before anything is resolved.

'ExtendsMalformed' is a value rather than a decoding failure on purpose: a
config whose @extends@ is a number is a different problem from a config that is
not JSON, and the loader reports them differently. Modelling it as a state keeps
that distinction in the type instead of in an error string.
-}
data ExtendsDto
    = ExtendsOne !Text
    | -- | Since TypeScript 5.0. Later entries win over earlier ones.
      ExtendsMany ![Text]
    | -- | Present, but neither a string nor an array of strings.
      ExtendsMalformed
    deriving (Show, Eq)

data CompilerOptionsDto = CompilerOptionsDto
    { baseUrl :: !(Maybe Text)
    , paths :: !(Maybe (Map Text [Text]))
    }
    deriving (Show, Eq, Generic)

instance FromJSON TsConfigDto
instance FromJSON CompilerOptionsDto

instance FromJSON ExtendsDto where
    parseJSON (String t) = pure $ ExtendsOne t
    parseJSON (Array xs) = pure . maybe ExtendsMalformed ExtendsMany . traverse text . toList $ xs
      where
        text (String t) = Just t
        text _ = Nothing
    parseJSON _ = pure ExtendsMalformed

{- | Decodes one config file's bytes.

Deliberately says nothing about /which/ file: only "TypeScript.Config.Loader"
knows that, and keeping it out means a config can be decoded and its failure
inspected without an absolute path getting into the answer.
-}
parseTsConfigJson :: ByteString -> Either Text TsConfigDto
parseTsConfigJson bs = do
    text <- bimap show stripTsComments . decodeUtf8' $ bs
    first T.pack . eitherDecode' . encodeUtf8 $ text

--------------------------------------------------------------------------------
-- Comment Stripping Logic
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

-- | Safely strips // and /* */ comments from a JSON string.
stripTsComments :: Text -> Text
stripTsComments input = fromMaybe input . parseMaybe jsoncStripper $ input

jsoncStripper :: Parser Text
jsoncStripper =
    T.concat
        <$> many
            ( stringLiteral
                <|> try lineComment
                <|> try blockComment
                <|> otherText
                <|> slash
            )
  where
    -- Safely consume string literals to protect URLs like "http://..."
    stringLiteral :: Parser Text
    stringLiteral = do
        start <- chunk "\""
        inner <- many (try escapedChar <|> normalStringChar)
        end <- chunk "\""
        pure $ start <> T.concat inner <> end

    escapedChar :: Parser Text
    escapedChar = do
        esc <- char '\\'
        c <- anySingle
        pure $ T.pack [esc, c]

    normalStringChar :: Parser Text
    normalStringChar = takeWhile1P Nothing (\c -> c /= '"' && c /= '\\')

    -- Strip out line comments
    lineComment :: Parser Text
    lineComment = do
        _ <- chunk "//"
        _ <- takeWhileP Nothing (/= '\n')
        pure ""

    -- Strip out block comments
    blockComment :: Parser Text
    blockComment = do
        _ <- chunk "/*"
        _ <- manyTill anySingle (chunk "*/")
        pure ""

    -- Bulk consume safe characters for performance
    otherText :: Parser Text
    otherText = takeWhile1P Nothing (\c -> c /= '"' && c /= '/')

    -- Catchall for isolated slashes
    slash :: Parser Text
    slash = chunk "/"
