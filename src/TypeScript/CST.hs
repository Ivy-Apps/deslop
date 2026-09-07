module TypeScript.CST (
    TsProgram (..),
    TsNode (..),
    specifierOf,
    withSpecifier,
) where

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
