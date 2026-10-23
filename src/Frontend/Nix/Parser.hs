{- | SPIKE. A crude, lossless "parser" for Nix: it keeps every byte and
scrapes only the dependency references a line-based scan can see.

Not a Nix parser. It recognises, per line:

* @import ./x.nix@ and @callPackage ./x.nix@ with a literal path,
* every literal path inside an @imports = [ ... ]@ list,
* @import <nixpkgs>@ lookup paths,
* @import (...)@, whose path is computed and only evaluation could name.
-}
module Frontend.Nix.Parser (
    NixUnit (..),
    NixRef (..),
    NixParseError (..),
    parseNix,
    renderNix,
    rewriteLine,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import FileSystem.Path (AbsPath (osPath), decodeOsPath)
import Renderable (Renderable (..))

-- | One @.nix@ file: its bytes, verbatim, and what the scan found in them.
data NixUnit = NixUnit
    { path :: AbsPath
    , source :: Text
    , refs :: [NixRef]
    }

-- | A dependency reference as written, with its 1-based line.
data NixRef
    = -- | A literal relative path: @./a.nix@, @../lib@.
      PathRef {line :: Int, written :: Text, code :: Text}
    | -- | A @<nixpkgs>@-style lookup path, resolved through @NIX_PATH@.
      LookupRef {line :: Int, written :: Text, code :: Text}
    | -- | A computed path; only evaluation knows what it names.
      DynamicRef {line :: Int, code :: Text}

-- | The only way the scan fails: bytes that are not UTF-8.
data NixParseError = NixParseError AbsPath Text

instance Renderable NixParseError where
    render (NixParseError p why) = decodeOsPath p.osPath <> ": " <> why

parseNix :: AbsPath -> ByteString -> Either NixParseError NixUnit
parseNix p = bimap (NixParseError p . show) (fromSource p) . TE.decodeUtf8'

fromSource :: AbsPath -> Text -> NixUnit
fromSource p src = NixUnit {path = p, source = src, refs = scanRefs src}

renderNix :: NixUnit -> ByteString
renderNix = TE.encodeUtf8 . (.source)

-- | Replaces @old@ with @new@ on one 1-based line, and rescans.
rewriteLine :: Int -> Text -> Text -> NixUnit -> NixUnit
rewriteLine n old new unit =
    fromSource unit.path . T.intercalate "\n" . zipWith edit [1 ..] . T.splitOn "\n" $ unit.source
  where
    edit i l = bool l (T.replace old new l) (i == n)

scanRefs :: Text -> [NixRef]
scanRefs = concat . snd . mapAccumL step False . zip [1 ..] . T.splitOn "\n"

-- | One line, given whether an @imports = [@ list is still open above it.
step :: Bool -> (Int, Text) -> (Bool, [NixRef])
step inList (n, raw)
    | "#" `T.isPrefixOf` T.stripStart raw = (inList, [])
    | otherwise = (active && not closes, calls <> listed <> dynamic)
  where
    code = T.strip raw
    opens = "imports" `T.isInfixOf` raw && "[" `T.isInfixOf` raw
    active = inList || opens
    closes = "]" `T.isInfixOf` raw
    toks = T.words . T.map (\c -> bool c ' ' (c `elem` ("[]{};" :: String))) $ raw
    pairs = zip toks (drop 1 toks)
    caller a = T.dropWhile (== '(') a `elem` ["import", "callPackage"]
    calledArgs = [b | (a, b) <- pairs, caller a]
    calls = mapMaybe call pairs
    call (a, b)
        | caller a && isPathLit (clean b) = Just $ PathRef n (clean b) code
        | caller a && "<" `T.isPrefixOf` b = Just $ LookupRef n (clean b) code
        | otherwise = Nothing
    listed =
        [ PathRef n (clean t) code
        | active
        , t <- toks
        , isPathLit (clean t)
        , t `notElem` calledArgs
        ]
    dynamic = [DynamicRef n code | "import (" `T.isInfixOf` raw, not . any (isPathLit . clean) $ calledArgs]
    clean = T.dropWhileEnd (`elem` (")," :: String))

isPathLit :: Text -> Bool
isPathLit t = any (`T.isPrefixOf` t) ["./", "../"] && t /= "./." && not ("+" `T.isInfixOf` t)
