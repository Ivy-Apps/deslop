{-# LANGUAGE QuasiQuotes #-}

module Translations.Parser (
    Translations (..),
    Translation (..),
    TransTree (..),
    LangCode,
    defaultLanguage,
    readTranslations,
    readTranslation,
    fkmap,
    (<.>),
) where

import Data.Aeson (Value (String), encode)
import Data.Aeson.Parser (value)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char, peekChar, skipSpace)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (intersperse, partition)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, listDirectory, readFileBS)
import FsEncoding (decodePathString)
import System.OsPath (OsPath, takeBaseName, (</>))
import Types (Renderable (..))
import Utils (safeHead)

type LangCode = Text

data Translations = Translations
    { base :: Translation
    , extra :: NonEmpty Translation
    }
    deriving (Show, Eq)

data Translation = Translation
    { language :: LangCode
    , tree :: TransTree
    }
    deriving (Show, Eq)

data TransTree
    = Root [TransTree]
    | Branch Text [TransTree]
    | Leaf Text Text
    deriving (Show, Eq)

defaultLanguage :: LangCode
defaultLanguage = "en"

readTranslations :: (RoFileSystem :> es) => OsPath -> Eff es (Maybe Translations)
readTranslations root =
    listDirectory root
        >>= traverse (readTranslation . (root </>))
        >>= pure . assemble . catMaybes
  where
    assemble :: [Translation] -> Maybe Translations
    assemble =
        uncurry (liftA2 Translations)
            . bimap safeHead nonEmpty
            . partition ((== defaultLanguage) . (.language))

readTranslation :: (RoFileSystem :> es) => OsPath -> Eff es (Maybe Translation)
readTranslation path =
    readFileBS path
        >>= pure . fmap (Translation language) . parseTransTree
  where
    language = T.pack . decodePathString . takeBaseName $ path

parseTransTree :: ByteString -> Maybe TransTree
parseTransTree bs = either (const Nothing) Just $ parseOnly rootParser bs
  where
    rootParser :: Parser TransTree
    rootParser =
        skipSpace
            >> char '{'
            >> parseChildren
            >>= pure . Root

    parseChildren :: Parser [TransTree]
    parseChildren = do
        skipSpace
        c <- peekChar
        case c of
            Just '}' -> char '}' >> pure []
            _ -> loop []
      where
        loop acc = do
            _ <- skipSpace
            key <- parseJsonString
            skipSpace >> char ':' >> skipSpace
            next <- peekChar
            node <- case next of
                Just '{' -> do
                    _ <- char '{'
                    Branch key <$> parseChildren
                Just '"' -> do
                    Leaf key <$> parseJsonString
                _ -> fail "NextJS translations must be Strings or Objects"

            skipSpace
            sep <- peekChar
            case sep of
                Just ',' -> char ',' >> loop (node : acc)
                Just '}' -> char '}' >> pure (reverse (node : acc))
                _ -> fail "Expected ',' or '}' in object definition"

    parseJsonString :: Parser Text
    parseJsonString = do
        v <- value
        case v of
            String t -> pure t
            _ -> fail "Expected JSON String Key/Value"

instance Renderable TransTree where
    render tree = (TL.toStrict . B.toLazyText $ renderNode 0 tree) <> "\n"
      where
        indentStep = 2
        mkIndent n = B.fromText (T.replicate n " ")
        escape = B.fromText . T.decodeUtf8 . BL.toStrict . encode
        renderNode lvl (Root children) = renderObj lvl children
        renderNode lvl (Branch k children) = escape k <> ": " <> renderObj lvl children
        renderNode _ (Leaf k v) = escape k <> ": " <> escape v
        renderObj _ [] = "{}"
        renderObj lvl children =
            "{"
                <> "\n"
                <> mconcat (intersperse ("," <> "\n") (map (renderChild (lvl + indentStep)) children))
                <> "\n"
                <> mkIndent lvl
                <> "}"
        renderChild lvl node = mkIndent lvl <> renderNode lvl node

fkmap :: (Text -> Text -> Text) -> TransTree -> TransTree
fkmap f = go ""
  where
    go p (Leaf k v) = Leaf k $ f (p <.> k) v
    go p (Root ts) = Root $ go p <$> ts
    go p (Branch k ts) = Branch k $ go (p <.> k) <$> ts

infixr 6 <.>
(<.>) :: Text -> Text -> Text
t1 <.> t2 = t1 <> "." <> t2
