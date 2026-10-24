{- | What a Nix file refers to, read from its lexemes without evaluating it.

Nix has no import statement worth the name: @import ./x.nix@, @imports = [
./x.nix ]@, @src = ./.@ and @builtins.readFile ./script.sh@ all depend on a
path the same way, by writing it. So every path literal outside a comment is a
'Reference', whatever it is used for, and so is every spelling of one that is
plainly a path:

* @.\/a.nix@, @..\/b@, @.\/plugins\/${name}.nix@ - from the file's directory;
* @"${root}\/modules\/x.nix"@ and @root + "\/modules"@, @root@ being one of the
  project's 'roots' - from the project root;
* @.\/dir + "\/${name}"@ - a literal base and a computed tail;
* @inputs.nixpkgs.lib@ - a flake input the root @flake.nix@ declares;
* @modulesPath + "\/profiles\/x.nix"@ - a NixOS module of nixpkgs;
* @\<nixpkgs\>@ - a search path entry.

A path built from something the lexemes cannot name - @dir + "\/${n}"@,
@builtins.readDir dir@ - is 'Computed'. Nothing else is guessed at: an
identifier is never taken for a path because of its name.

Reading is lexical, so it sees what is written, not what is bound: a @root@
that a @let@ or a lambda shadows is still read as the project root. See
@docs\/adr\/0023@.
-}
module Frontend.Nix.Reference (
    Scope (..),
    Reference (..),
    Referent (..),
    Anchor (..),
    PathPattern,
    Segment,
    Piece (..),
    literalOf,
    referencesIn,
) where

import Data.List (groupBy)
import Data.Set qualified as Set
import Data.Text qualified as T
import Frontend.Nix.Lexeme (Lexeme (..), Part (..), Quoting (..), decodeChunk, interpolations, isTrivia)
import Renderable qualified

-- | The names a file's references are read against.
data Scope = Scope
    { roots :: Set Text
    -- ^ What names the project root: @self@, and whatever @deslop\/nix.yaml@ adds.
    , inputs :: Set Text
    -- ^ The flake inputs the root @flake.nix@ declares.
    }
    deriving stock (Show, Eq)

data Reference = Reference
    { offset :: Int
    -- ^ Characters from the start of the file to where it is written.
    , written :: Text
    -- ^ As the author wrote it.
    , referent :: Referent
    }
    deriving stock (Show, Eq)

data Referent
    = -- | A file or directory, found where the 'Anchor' says.
      LocalPath Anchor PathPattern
    | {- | @inputs.\<name\>.\<attr\>...@, the attributes up to the first one
      that is computed.
      -}
      FlakeInput Text [Text]
    | -- | @modulesPath + "\/..."@: a module of nixpkgs' @nixos\/modules@.
      NixosModule PathPattern
    | -- | @\<nixpkgs\/lib\>@, by its segments.
      SearchPathEntry [Text]
    | -- | A path only evaluation names.
      Computed
    deriving stock (Show, Eq)

-- | Where a path is spelled from.
data Anchor
    = FromFile
    | FromRoot
    | -- | @\/etc\/hosts@ or @~\/x@: a path no project holds.
      Elsewhere
    deriving stock (Show, Eq)

-- | A path by its segments; a segment with a 'Hole' stands for every name it matches.
type PathPattern = [Segment]

type Segment = [Piece]

data Piece
    = Literal Text
    | -- | What an interpolation computes.
      Hole
    deriving stock (Show, Eq)

-- | A segment's text, unless an interpolation computes part of it.
literalOf :: Segment -> Maybe Text
literalOf = fmap mconcat . traverse literal
  where
    literal = \case
        Literal text -> Just text
        Hole -> Nothing

-- | Every reference in a file, in the order they are written.
referencesIn :: Scope -> [Lexeme] -> [Reference]
referencesIn scope lexemes = sortOn (.offset) (inCode scope source (located 0 lexemes))
  where
    source = Renderable.render lexemes

-- | A lexeme that is not trivia, and the characters it spans.
data Located = Located
    { start :: Int
    , end :: Int
    , lexeme :: Lexeme
    }

located :: Int -> [Lexeme] -> [Located]
located offset = filter (not . isTrivia . (.lexeme)) . snd . mapAccumL place offset
  where
    place o l = let o' = o + T.length (Renderable.render l) in (o', Located o o' l)

-- | The references of one run of code, and of the code interpolated in it.
inCode :: Scope -> Text -> [Located] -> [Reference]
inCode scope source code = go Nothing code <> concatMap interpolated code
  where
    go previous = \case
        [] -> []
        t : rest -> case recognise scope (placeAfter previous) t rest of
            Just construct ->
                maybe id ((:) . reference t.start construct.end) construct.value (go (Just t.lexeme) construct.rest)
            Nothing -> go (Just t.lexeme) rest
    reference start end = Reference start (T.take (end - start) (T.drop start source))

    placeAfter = \case
        Just (Punctuation ".") -> Attribute
        _ -> Term

    interpolated t = concatMap (\(o, lexemes) -> inCode scope source (located o lexemes)) (interpolations t.start t.lexeme)

-- | Where a lexeme stands: @inputs@ in @x.inputs@ names an attribute, not the flake's inputs.
data Place
    = -- | After a @.@, as the name of an attribute.
      Attribute
    | Term
    deriving stock (Eq)

-- | What a run of lexemes read as, where the run ends, and what follows it.
data Spanning a = Spanning
    { value :: a
    , end :: Int
    , rest :: [Located]
    }
    deriving stock (Functor)

{- | The construct starting at this lexeme, if one does, and what it refers
to: 'Nothing' for one that refers to nothing, such as @.\/.@.
-}
recognise :: Scope -> Place -> Located -> [Located] -> Maybe (Spanning (Maybe Referent))
recognise scope place t rest = case t.lexeme of
    PathLit parts -> Just $ case (appended rest, rest) of
        (Just strings, _) -> Just . localPath (anchorOf parts) . (pathPieces parts <>) <$> strings
        (Nothing, plus : operand : afterOperand)
            | plus.lexeme == Punctuation "+"
            , Just name <- selection (operand : afterOperand) ->
                Just Computed <$ name
        _ -> Spanning (localPathUnlessHere (anchorOf parts) (pathPieces parts)) t.end rest
    Str quoting (Interpolation inner : parts)
        | place == Term
        , Just name <- soleIdentifier inner
        , name `Set.member` scope.roots ->
            Just $ fromRoot . (stringPieces quoting parts <>) <$> fromMaybe (Spanning [] t.end rest) (appended rest)
    SearchPath inner -> Just (Spanning (Just (SearchPathEntry (T.splitOn "/" inner))) t.end rest)
    Identifier name
        | place == Attribute -> Nothing
        | name `Set.member` scope.roots -> fmap fromRoot <$> appended rest
        | name == "modulesPath" -> fmap (Just . NixosModule . segments) <$> appended rest
        | name == "inputs" -> flakeInput
        | name `elem` ["import", "builtins"] -> readOf
        | otherwise -> computedConcat
    _ -> Nothing
  where
    flakeInput = do
        chain <- selection (t : rest)
        (name, attrs) <- case chain.value of
            _ : name : attrs -> Just (name, attrs)
            _ -> Nothing
        guard (name `Set.member` scope.inputs)
        -- @inputs.nixpkgs.follows = ...@ binds an attribute; it uses nothing.
        guard (next chain /= Just (Punctuation "="))
        pure (Just (FlakeInput name attrs) <$ chain)

    -- @import x@ and @builtins.readDir x@, @x@ a name rather than a path.
    readOf = do
        callee <- selection (t : rest)
        guard (callee.value `elem` [["import"], ["builtins", "readDir"], ["builtins", "readFile"], ["builtins", "pathExists"]])
        operand <- selection callee.rest
        guard (next operand `notElem` [Just (Punctuation "+"), Just (Punctuation ".")])
        guard (viaNonEmpty head operand.value `notElem` fmap Just ("inputs" : toList scope.roots))
        pure (Just Computed <$ operand)

    -- @dir + "/x"@: a path, built from a name no lexeme spells.
    computedConcat = do
        name <- selection (t : rest)
        strings <- appended name.rest
        case strings.value of
            Literal text : _ | "/" `T.isPrefixOf` text -> pure (Just Computed <$ strings)
            _ -> Nothing

    next spanning = (.lexeme) <$> viaNonEmpty head spanning.rest

{- | @+ "a" + "b"@: the pieces the strings spell. 'Nothing' when no string is
appended.
-}
appended :: [Located] -> Maybe (Spanning [Piece])
appended = \case
    plus : Located {lexeme = Str quoting parts, end} : rest
        | plus.lexeme == Punctuation "+" ->
            Just $ (stringPieces quoting parts <>) <$> fromMaybe (Spanning [] end rest) (appended rest)
    _ -> Nothing

-- | @a.b.c@: the names, up to the first attribute that is not one.
selection :: [Located] -> Maybe (Spanning [Text])
selection = \case
    Located {lexeme = Identifier name, end} : rest -> Just (go [name] end rest)
    _ -> Nothing
  where
    go names end = \case
        dot : Located {lexeme = Identifier name, end = end'} : rest
            | dot.lexeme == Punctuation "." -> go (names <> [name]) end' rest
        rest -> Spanning names end rest

soleIdentifier :: [Lexeme] -> Maybe Text
soleIdentifier lexemes = case filter (not . isTrivia) lexemes of
    [Identifier name] -> Just name
    _ -> Nothing

anchorOf :: [Part] -> Anchor
anchorOf = \case
    Chunk text : _ | "/" `T.isPrefixOf` text || "~" `T.isPrefixOf` text -> Elsewhere
    _ -> FromFile

localPath :: Anchor -> [Piece] -> Referent
localPath anchor = LocalPath anchor . segments

-- | @.\/.@ is the file's own directory, which every file has: no dependency.
localPathUnlessHere :: Anchor -> [Piece] -> Maybe Referent
localPathUnlessHere anchor pieces = case (anchor, segments pieces) of
    (FromFile, pattern) | all (== [Literal "."]) pattern -> Nothing
    (_, pattern) -> Just (LocalPath anchor pattern)

{- | What follows @${root}@ or @root +@: a path from the root when it starts
with a slash, nothing for the root itself, and a computed path otherwise -
@"${root}x"@ names no file of the project.
-}
fromRoot :: [Piece] -> Maybe Referent
fromRoot = \case
    [] -> Nothing
    pieces@(Literal text : _)
        | "/" `T.isPrefixOf` text -> case segments pieces of
            [] -> Nothing
            pattern -> Just (LocalPath FromRoot pattern)
    _ -> Just Computed

pathPieces :: [Part] -> [Piece]
pathPieces = fmap $ \case
    Chunk text -> Literal text
    Interpolation _ -> Hole

stringPieces :: Quoting -> [Part] -> [Piece]
stringPieces quoting = fmap $ \case
    Chunk text -> Literal (decodeChunk quoting text)
    Interpolation _ -> Hole

-- | Split at every slash, without the empty segments a leading or doubled slash leaves.
segments :: [Piece] -> PathPattern
segments = filter (not . null) . fmap pieces . splitAtSlashes . concatMap characters
  where
    characters = \case
        Literal text -> Left <$> toString text
        Hole -> [Right ()]
    splitAtSlashes xs = case break (== Left '/') xs of
        (segment, []) -> [segment]
        (segment, _ : rest) -> segment : splitAtSlashes rest
    pieces = fmap piece . groupBy (\a b -> isLeft a && isLeft b)
    piece = \case
        run@(Left _ : _) -> Literal (toText (lefts run))
        _ -> Hole
