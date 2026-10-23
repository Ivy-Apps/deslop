{- | SPIKE: a crude, lossless "parser" for Python.

It keeps every physical line verbatim and recognises only top-level (column 0)
@import@ and @from ... import@ statements written on one line. Indented imports
(@if TYPE_CHECKING:@, function-local), parenthesised multi-line imports,
backslash continuations, @;@-separated statements and @importlib@ calls are
all invisible to it.
-}
module Frontend.Python.Parser (
    PyUnit (..),
    PyLine (..),
    PyImport (..),
    PyParseError (..),
    parsePy,
    renderPy,
    importedNames,
) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import FileSystem.Path (AbsPath (..), decodeOsPath)
import Renderable (Renderable (..))

-- | One @.py@ file, as the lines it was read from.
data PyUnit = PyUnit
    { path :: AbsPath
    , lines :: [PyLine]
    }

-- | A physical line, verbatim, and the import it states when it states one.
data PyLine = PyLine
    { raw :: ByteString
    , stmt :: Maybe PyImport
    }

data PyImport
    = -- | @import a.b.c, d as e@: every dotted module named.
      ImportModules [Text]
    | -- | @from ..pkg.mod import x, y as z@: the leading-dot level, the
      -- module's dotted segments, and everything after @import@ verbatim.
      FromImport
        { level :: Int
        , modSegs :: [Text]
        , afterImport :: Text
        }

-- | A line that is not UTF-8. Python would honour a PEP 263 coding cookie.
data PyParseError = PyParseError AbsPath Int

instance Renderable PyParseError where
    render (PyParseError p line) =
        decodeOsPath p.osPath <> ":" <> show line <> ": not UTF-8 (PEP 263 coding cookies are unsupported)"

parsePy :: AbsPath -> ByteString -> Either PyParseError PyUnit
parsePy p = fmap (PyUnit p) . traverse lineOf . zip [1 ..] . BS.split 10
  where
    lineOf (n, bytes) = case decodeUtf8' bytes of
        Left _ -> Left $ PyParseError p n
        Right t -> Right . PyLine bytes . importOf $ t

-- | The inverse of 'parsePy'.
renderPy :: PyUnit -> ByteString
renderPy = BS.intercalate "\n" . fmap (.raw) . (.lines)

importOf :: Text -> Maybe PyImport
importOf line = case T.stripPrefix "import " code of
    Just rest -> Just . ImportModules . fmap firstWord . T.splitOn "," $ rest
    Nothing -> fromImport =<< T.stripPrefix "from " code
  where
    code = T.stripEnd . T.takeWhile (/= '#') . T.dropWhileEnd (== '\r') $ line
    fromImport rest =
        let (modPart, after) = T.breakOn " import " rest
            dotted = T.strip modPart
            dots = T.length . T.takeWhile (== '.') $ dotted
            name = T.drop dots dotted
         in guard (not (T.null after))
                $> FromImport
                    { level = dots
                    , modSegs = filter (not . T.null) . T.splitOn "." $ name
                    , afterImport = T.drop (T.length " import ") after
                    }

-- | The names a @from ... import@ binds, before any @as@.
importedNames :: Text -> [Text]
importedNames = filter (not . T.null) . fmap firstWord . T.splitOn "," . T.filter (`notElem` ("()" :: String)) . T.takeWhile (/= '#')

firstWord :: Text -> Text
firstWord = fromMaybe "" . viaNonEmpty head . words
