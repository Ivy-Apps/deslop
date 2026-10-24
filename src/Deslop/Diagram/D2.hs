{- | The slice of the D2 diagram language Deslop writes, and its rendering to
source text. Knows D2's syntax and nothing about modules: what to draw is
"Deslop.Diagram".

Every name a project supplies is written as a double-quoted 'Name' segment.
Unquoted, D2 reads @.@ as nesting, and @[id]@, @\@scope@, @--@, @->@ and its
reserved words (@label@, @style@, @shape@, @link@, @_@) break or misparse - all
of which a file or folder name can contain. Only D2's own vocabulary is a
'Keyword', which is written bare.

Quoting is not quite enough. D2 still reads a quoted @layers@, @scenarios@ or
@steps@ as a board of its own, whatever its case, so such a 'Name' carries an
invisible zero-width space to tell it apart.
-}
module Deslop.Diagram.D2 (
    Document (..),
    Statement (..),
    Key (..),
    Segment (..),
    Value (..),
    keyword,
    name,
    render,
    quote,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

-- | A whole @.d2@ file.
newtype Document = Document [Statement]
    deriving stock (Show, Eq)

data Statement
    = -- | @key: value@
      Field Key Value
    | -- | @key: { ... }@, or a bare @key@ declaring a shape with nothing set.
      Block Key [Statement]
    | -- | @from -> to: { ... }@
      Connection Key Key [Statement]
    deriving stock (Show, Eq)

-- | A dotted path: each segment one level deeper into a container.
newtype Key = Key (NonEmpty Segment)
    deriving stock (Show, Eq)

data Segment
    = -- | D2's own vocabulary - @shape@, @style@, @layers@ - written bare.
      Keyword Text
    | -- | A name the project supplied, always quoted.
      Name Text
    deriving stock (Show, Eq)

data Value
    = -- | Written as it stands: a number, @package@, a board path like @_@.
      Bare Text
    | -- | A string the project supplied, always quoted.
      Str Text
    deriving stock (Show, Eq)

-- | A key spelled in D2's own vocabulary, e.g. @keyword ["style", "stroke-width"]@.
keyword :: NonEmpty Text -> Key
keyword = Key . fmap Keyword

-- | A key naming a nested shape by the project's own names.
name :: NonEmpty Text -> Key
name = Key . fmap Name

render :: Document -> Text
render (Document statements) = T.unlines $ concatMap (renderStatement 0) statements

renderStatement :: Int -> Statement -> [Text]
renderStatement depth = \case
    Field k v -> [indent <> renderKey k <> ": " <> renderValue v]
    Block k body -> braced (renderKey k) body
    Connection from to body -> braced (renderKey from <> " -> " <> renderKey to) body
  where
    indent = T.replicate depth "  "
    braced header [] = [indent <> header]
    braced header body =
        [indent <> header <> ": {"]
            <> concatMap (renderStatement $ depth + 1) body
            <> [indent <> "}"]

renderKey :: Key -> Text
renderKey (Key segments) = T.intercalate "." . NE.toList . fmap renderSegment $ segments
  where
    renderSegment (Keyword k) = k
    renderSegment (Name n)
        | T.toLower n `elem` boardKeywords = quote $ n <> zeroWidthSpace
        | otherwise = quote n

    boardKeywords = ["layers", "scenarios", "steps"]
    zeroWidthSpace = "\x200B"

renderValue :: Value -> Text
renderValue (Bare v) = v
renderValue (Str s) = quote s

-- | A double-quoted D2 string, with @\"@ and @\\@ backslash-escaped.
quote :: Text -> Text
quote t = "\"" <> T.concatMap escape t <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = T.singleton c
