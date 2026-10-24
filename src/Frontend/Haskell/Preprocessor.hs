{- | What GHC runs over a file before it reads it, and who asked for it.

A file a preprocessor rewrites is not the file GHC compiles, so Deslop cannot
read its imports from its bytes. Two are recognised: @CPP@, and @-F -pgmF@
programs, of which @hspec-discover@ is the one whose output Deslop knows -
see "Frontend.Haskell.HspecDiscover". Either is asked for in the file's own
header pragmas or by its component in the @.cabal@ file.
-}
module Frontend.Haskell.Preprocessor (
    Preprocessor (..),
    Request (..),
    RequestedBy (..),
    requestedInFile,
    requestedByOptions,
    requestedByExtensions,
    opaqueRequest,
) where

import Data.Char (isSpace, toUpper)
import Data.Text qualified as T
import Frontend.Haskell.Tokens (HsToken (..), HsTokenKind (..), Positioned (..), isTrivia, positioned)
import System.FilePath (takeBaseName)

data Preprocessor
    = Cpp
    | HspecDiscover
    | -- | Any other @-F -pgmF@ program, named as written.
      Custom Text
    deriving stock (Show, Eq)

data Request = Request
    { preprocessor :: Preprocessor
    , by :: RequestedBy
    }
    deriving stock (Show, Eq)

data RequestedBy
    = -- | A @LANGUAGE@ or @OPTIONS_GHC@ pragma: its 1-based line and its text.
      ByPragma Int Text
    | {- | A component's @default-extensions@ or @ghc-options@, named
      @lib:deslop@-style.
      -}
      ByComponent Text
    deriving stock (Show, Eq)

{- | The preprocessors the file's header pragmas ask for. Only pragmas before
the first token that is not trivia count, which is where GHC reads them.
-}
requestedInFile :: [HsToken] -> [Request]
requestedInFile = concatMap requests . pragmasOnLines . takeWhile isTrivia
  where
    requests (line, t) = (`Request` ByPragma line (T.strip t.raw)) <$> pragmaPreprocessors t.raw

pragmasOnLines :: [HsToken] -> [(Int, HsToken)]
pragmasOnLines toks = [(p.line, p.token) | p <- positioned toks, p.token.kind == PragmaK]

-- | @{\-# LANGUAGE CPP #-\}@, @{\-# OPTIONS_GHC -F -pgmF hspec-discover #-\}@.
pragmaPreprocessors :: Text -> [Preprocessor]
pragmaPreprocessors raw = case T.words inner of
    name : rest
        | T.map toUpper name == "LANGUAGE" -> fromExtensions (T.splitOn "," (unwords rest))
        | T.map toUpper name `elem` ["OPTIONS_GHC", "OPTIONS"] -> fromOptions rest
    _ -> []
  where
    inner = T.dropEnd 3 . T.drop 3 $ raw

requestedByExtensions :: Text -> [Text] -> [Request]
requestedByExtensions component = fmap (`Request` ByComponent component) . fromExtensions

requestedByOptions :: Text -> [Text] -> [Request]
requestedByOptions component = fmap (`Request` ByComponent component) . fromOptions

fromExtensions :: [Text] -> [Preprocessor]
fromExtensions = ([Cpp] <*) . filter (== "CPP") . fmap (T.filter (not . isSpace))

{- | GHC flags: @-XCPP@ and @-cpp@ run CPP; @-pgmF prog@ names a preprocessor
that only @-F@ runs.
-}
fromOptions :: [Text] -> [Preprocessor]
fromOptions flags = cpp <> pgmF
  where
    cpp = [Cpp | any (`elem` ["-XCPP", "-cpp"]) flags]
    pgmF = [program p | "-F" `elem` flags, p <- programs flags]
    programs = \case
        "-pgmF" : p : rest -> p : programs rest
        flag : rest -> maybeToList (T.stripPrefix "-pgmF" flag >>= nonEmptyText) <> programs rest
        [] -> []
    nonEmptyText t = t <$ guard (not (T.null t))
    program p
        | takeBaseName (toString p) == "hspec-discover" = HspecDiscover
        | otherwise = Custom p

{- | The first request for a preprocessor whose output Deslop cannot know: the
file is read as an opaque island. That is any but hspec-discover asked for by
the file's own pragma, the one driver whose output Deslop knows; asked for by
a component, it would rewrite every file of it.
-}
opaqueRequest :: [Request] -> Maybe Request
opaqueRequest = find $ \r -> case (r.preprocessor, r.by) of
    (HspecDiscover, ByPragma _ _) -> False
    _ -> True
