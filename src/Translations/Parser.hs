{-# OPTIONS_GHC -Wno-orphans #-}

module Translations.Parser where

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
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Effectful (Eff, (:>))
import Effects.FileSystem (RoFileSystem, listDirectory, readFileBS)
import System.FilePath (takeBaseName, (</>))
import Utils (safeHead)

type Language = Text

data Translations = Translations
    { base :: Translation
    , extra :: NonEmpty Translation
    }
    deriving (Show, Eq)

data Translation = Translation
    { language :: Language
    , tree :: TransTree
    }
    deriving (Show, Eq)

data TransTree
    = Root [TransTree]
    | Branch Text [TransTree]
    | Leaf Text Text
    deriving (Show, Eq)

defaultLanguage :: Text
defaultLanguage = "en"

readTranslations :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe Translations)
readTranslations root =
    listDirectory root
        >>= traverse readTranslation . fmap (root </>)
        >>= pure . assemble . catMaybes
  where
    assemble :: [Translation] -> Maybe Translations
    assemble =
        uncurry (liftA2 Translations)
            . bimap safeHead nonEmpty
            . partition ((== defaultLanguage) . (.language))

readTranslation :: (RoFileSystem :> es) => FilePath -> Eff es (Maybe Translation)
readTranslation path =
    readFileBS path
        >>= pure . fmap (Translation language) . parseTransTree
  where
    language = T.pack . takeBaseName $ path

parseTransTree :: ByteString -> Maybe TransTree
parseTransTree bs = either (const Nothing) Just $ parseOnly rootParser bs
  where
    rootParser :: Parser TransTree
    rootParser = do
        skipSpace
        char '{'
        children <- parseChildren
        pure $ Root children

    parseChildren :: Parser [TransTree]
    parseChildren = do
        skipSpace
        c <- peekChar
        case c of
            Just '}' -> char '}' >> pure []
            _ -> loop []
      where
        loop acc = do
            skipSpace
            key <- parseJsonString
            skipSpace >> char ':' >> skipSpace
            next <- peekChar
            node <- case next of
                Just '{' -> do
                    char '{'
                    kids <- parseChildren
                    pure $ Branch key kids
                Just '"' -> do
                    val <- parseJsonString
                    pure $ Leaf key val
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

render :: TransTree -> Text
render tree = TL.toStrict . toLazyText $ renderNode 0 tree
  where
    indentStep :: Int
    indentStep = 2

    mkIndent :: Int -> Builder
    mkIndent n = fromText (T.replicate n " ")

    escape :: Text -> Builder
    escape = fromText . T.decodeUtf8 . BL.toStrict . encode

    renderNode :: Int -> TransTree -> Builder
    renderNode lvl (Root children) =
        renderObj lvl children
    renderNode lvl (Branch k children) =
        escape k <> ": " <> renderObj lvl children
    renderNode _ (Leaf k v) =
        escape k <> ": " <> escape v

    renderObj :: Int -> [TransTree] -> Builder
    renderObj _ [] = "{}"
    renderObj lvl children =
        "{"
            <> "\n"
            <> mconcat (intersperse ("," <> "\n") (map (renderChild (lvl + indentStep)) children))
            <> "\n"
            <> mkIndent lvl
            <> "}"

    renderChild :: Int -> TransTree -> Builder
    renderChild lvl node = mkIndent lvl <> renderNode lvl node