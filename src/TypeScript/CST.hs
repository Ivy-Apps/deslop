module TypeScript.CST (
    TsProgram (..),
    TsNode (..),
    specifierOf,
    withSpecifier,
    onLines,
) where

import Data.Text qualified as T
import FileSystem.Path (AbsPath)
import Renderable (Renderable (..))

data TsProgram = TsModule
    { path :: AbsPath
    , cst :: [TsNode]
    }
    deriving (Show, Eq)

{- | One piece of a TypeScript file, split so that a specifier can be rewritten
without touching anything around it. 'prefix' and 'suffix' are the statement
verbatim on either side of the specifier, so rendering is lossless.
-}
data TsNode
    = Import
        { prefix :: Text
        , target :: Text
        , suffix :: Text
        }
    | -- | @export ... from "..."@. Its target is a dependency exactly as an
      -- import's is; what differs is that the module re-exposes it.
      ReExport
        { prefix :: Text
        , target :: Text
        , suffix :: Text
        }
    | Source
        { raw :: Text
        }
    deriving (Show, Eq)

-- | The module a node names, for the nodes that name one.
specifierOf :: TsNode -> Maybe Text
specifierOf (Import _ t _) = Just t
specifierOf (ReExport _ t _) = Just t
specifierOf (Source _) = Nothing

-- | Replaces the specifier, leaving every other byte of the statement alone.
withSpecifier :: Text -> TsNode -> TsNode
withSpecifier t node@Import {} = node {target = t}
withSpecifier t node@ReExport {} = node {target = t}
withSpecifier _ node@Source {} = node

instance Renderable TsNode where
    render (Source r) = r
    render (Import p t s) = p <> t <> s
    render (ReExport p t s) = p <> t <> s

{- | Each node paired with the 1-based line its first character sits on.

Derived rather than lexed. Rendering reproduces the source byte for byte -
pinned by P1 in "TypeScript.ParserPropSpec" - so counting newlines across the
nodes before one gives the same answer the lexer would have, and costs the
lexer nothing to carry.
-}
onLines :: [TsNode] -> [(Int, TsNode)]
onLines nodes = zip (scanl step 1 nodes) nodes
  where
    step line = (line +) . T.count "\n" . render
