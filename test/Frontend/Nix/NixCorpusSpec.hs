{- | The differential test: Deslop's Nix lexer must find the same path
literals and search paths in a file that Nix's own parser does.

The answers in @fixtures\/nix\/nix-corpus.json@ are the real parser's, taken by
@just update-nix-corpus@ with @nix-instantiate --parse@ and committed - so this
runs against an oracle without needing Nix on the machine. See
@docs\/adr\/0024-nix-paths-are-judged-against-nix-instantiate.md@.

Nix resolves a path literal while it parses, so what it says is the literal
made absolute, and of an interpolated one only the text before its first
@${@. Each literal the lexer finds is resolved the same way before the two are
compared, as sorted lists: Nix prints an attribute set sorted by name, so the
order of its answer is not the order of the file.

A case carrying a @knownGap@ is one Deslop does not yet read the way Nix
does; see 'TestUtils.corpusCase'.
-}
module Frontend.Nix.NixCorpusSpec (spec) where

import Data.Aeson (FromJSON (..), Options (..), SumEncoding (..), defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Text qualified as T
import Fixtures.OnDisk (nixCorpus)
import Frontend.Nix.Lexeme (Lexeme (..), interpolations)
import Frontend.Nix.Lexer (PathLiteralSpan (..), lexNix, pathLiteralSpans)
import Test.Hspec
import TestUtils (corpusCase, loadCorpus)

-- | One case, exactly as authored and recorded in the corpus file.
data NixCase = NixCase
    { name :: Text
    , source :: Text
    , nix :: Verdict
    , knownGap :: Maybe Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

-- | What @nix-instantiate --parse@ made of a case.
data Verdict
    = Parses Literals
    | -- | The error it stopped at.
      Rejects Text
    deriving stock (Generic, Show)

instance FromJSON Verdict where
    parseJSON = genericParseJSON defaultOptions {sumEncoding = ObjectWithSingleField, constructorTagModifier = fmap toLower}

-- | Every path literal, resolved, and every search path of a file, each sorted.
data Literals = Literals
    { paths :: [Text]
    , searchPaths :: [Text]
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON)

{- | Either side's answer, without what only Nix can say: why a file does not
parse.
-}
data Reading
    = Reads Literals
    | DoesNotRead
    deriving stock (Show, Eq)

spec :: Spec
spec = describe "Frontend.Nix.NixCorpus" $ do
    cases <- runIO (loadCorpus @NixCase nixCorpus)
    for_ cases $ \c ->
        corpusCase c.name c.knownGap (pure (lexerReading c.source)) (nixReading c.nix)

nixReading :: Verdict -> Reading
nixReading = \case
    Parses literals -> Reads literals
    Rejects _ -> DoesNotRead

lexerReading :: Text -> Reading
lexerReading source = maybe DoesNotRead Reads $ do
    spans <- pathLiteralSpans source
    lexemes <- lexNix source
    pure
        Literals
            { paths = sort (resolve . fst . T.breakOn "${" . (.literal) <$> spans)
            , searchPaths = sort (searchPathsIn lexemes)
            }

{- | Where the corpus says a case lives: @\/corpus\/project\/case.nix@, with
@HOME@ at @\/corpus\/home@.
-}
projectDir, homeDir :: Text
projectDir = "/corpus/project"
homeDir = "/corpus/home"

{- | A path literal's text before its first interpolation, as Nix's parser
resolves it: a home path is appended to @HOME@ as written; any other is made
absolute against the file's directory and canonicalised, keeping a trailing
slash only where an interpolation follows it.
-}
resolve :: Text -> Text
resolve literal = case T.stripPrefix "~" literal of
    Just rest -> homeDir <> rest
    Nothing -> keepTrailingSlash . canonical $ if "/" `T.isPrefixOf` literal then literal else projectDir <> "/" <> literal
  where
    keepTrailingSlash path
        | T.length literal > 1 && "/" `T.isSuffixOf` literal = path <> "/"
        | otherwise = path
    canonical = ("/" <>) . T.intercalate "/" . reverse . foldl' step [] . T.splitOn "/"
    step segments = \case
        "" -> segments
        "." -> segments
        ".." -> drop 1 segments
        segment -> segment : segments

-- | Every search path, including those inside an interpolation.
searchPathsIn :: [Lexeme] -> [Text]
searchPathsIn = foldMap $ \case
    SearchPath inner -> [inner]
    lexeme -> foldMap (searchPathsIn . snd) (interpolations 0 lexeme)
