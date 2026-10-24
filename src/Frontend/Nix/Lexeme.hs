{- | The lexemes Deslop splits Nix source into.

Lossless: every character of a file is in exactly one lexeme, so rendering
them gives the file back. Only as fine-grained as finding what a file depends
on needs: a string and a path keep their interpolations, because what is
written inside @${ }@ is code, and code can hold a path.
-}
module Frontend.Nix.Lexeme (
    Lexeme (..),
    Quoting (..),
    Part (..),
    isTrivia,
    nesting,
    withOffsets,
    interpolations,
    decodeChunk,
) where

import Data.Text qualified as T
import Renderable (Renderable (..))

data Lexeme
    = -- | Whitespace, a @#@ comment or a @\/* *\/@ comment.
      Trivia Text
    | {- | @foo@, @mkDerivation'@, @home-manager@ - and the keywords, which
      Nix spells as identifiers.
      -}
      Identifier Text
    | -- | An operator or a delimiter: @.@, @=@, @+@, @{@, @${@, @//@.
      Punctuation Text
    | -- | A string, its delimiters left to the 'Quoting'.
      Str Quoting [Part]
    | {- | A path literal, @.\/a.nix@, @..\/b@, @\/etc\/hosts@, @~\/x@, or one
      with interpolations, @.\/plugins\/${name}.nix@.
      -}
      PathLit [Part]
    | -- | @<nixpkgs\/lib>@, holding what is between the angle brackets.
      SearchPath Text
    | -- | @https:\/\/example.org@, an unquoted URI.
      Uri Text
    | -- | A number, or a character no other lexeme claims.
      Other Text
    deriving stock (Show, Eq)

-- | How a string is delimited, which decides how its text is escaped.
data Quoting
    = -- | @"..."@
      DoubleQuoted
    | -- | @''...''@
      Indented
    deriving stock (Show, Eq)

-- | A run of a string's or a path's own text, or an interpolation in it.
data Part
    = -- | Verbatim, escapes included; see 'decodeChunk'.
      Chunk Text
    | -- | @${ ... }@, holding the code between the braces.
      Interpolation [Lexeme]
    deriving stock (Show, Eq)

-- | What grammar looks straight through.
isTrivia :: Lexeme -> Bool
isTrivia = \case
    Trivia _ -> True
    _ -> False

-- | How many braces a lexeme opens, or closes when negative: @${@ is one.
nesting :: Lexeme -> Int
nesting = \case
    Punctuation "{" -> 1
    Punctuation "${" -> 1
    Punctuation "}" -> -1
    _ -> 0

{- | Each one with the offset it starts at, given the offset the first starts
at: what it renders to is what it spans.
-}
withOffsets :: (Renderable a) => Int -> [a] -> [(Int, a)]
withOffsets start = snd . mapAccumL (\o x -> (o + T.length (render x), (o, x))) start

{- | The code of every interpolation written in a string or a path, each with
the offset it starts at, given the offset the lexeme starts at: what is
inside @${ }@ is code, and can hold a path of its own.
-}
interpolations :: Int -> Lexeme -> [(Int, [Lexeme])]
interpolations start = \case
    Str DoubleQuoted parts -> inParts (start + 1) parts
    Str Indented parts -> inParts (start + 2) parts
    PathLit parts -> inParts start parts
    _ -> []
  where
    inParts offset parts = [(o + 2, lexemes) | (o, Interpolation lexemes) <- withOffsets offset parts]

{- | The text a chunk of a string stands for, its escapes undone: @\\"@ is
@"@ in a double-quoted string, and @''$@ is @$@ in an indented one.
-}
decodeChunk :: Quoting -> Text -> Text
decodeChunk quoting = toText . go . toString
  where
    go = case quoting of
        DoubleQuoted -> \case
            '\\' : c : rest -> escaped c <> go rest
            c : rest -> c : go rest
            [] -> []
        Indented -> \case
            '\'' : '\'' : '\'' : rest -> "''" <> go rest
            '\'' : '\'' : '$' : rest -> '$' : go rest
            '\'' : '\'' : '\\' : c : rest -> escaped c <> go rest
            c : rest -> c : go rest
            [] -> []
    escaped = \case
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        c -> [c]

instance Renderable Lexeme where
    render = \case
        Trivia t -> t
        Identifier t -> t
        Punctuation t -> t
        Str DoubleQuoted parts -> "\"" <> foldMap render parts <> "\""
        Str Indented parts -> "''" <> foldMap render parts <> "''"
        PathLit parts -> foldMap render parts
        SearchPath t -> "<" <> t <> ">"
        Uri t -> t
        Other t -> t

instance Renderable Part where
    render = \case
        Chunk t -> t
        Interpolation lexemes -> "${" <> render lexemes <> "}"
