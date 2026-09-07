{- | Lowering a TypeScript concrete syntax tree into the language-agnostic
"Deslop.AST".

This is the seam a language crosses: everything above it reasons about
'AstModule's and knows nothing of TypeScript, and everything below it is
TypeScript all the way down to the bytes. A second language earns its support
by supplying a module of this shape and nothing more.

It is also the only place a specifier is resolved. That matters: while the graph
took its edges from whatever the lint pass left in the CST, baselining a lint
Problem silently changed the graph. Resolving here makes the two independent.
-}
module TypeScript.AST (parseAst) where

import Deslop.AST (
    AstModule (..),
    AstNode (..),
    EdgeKind (..),
    EdgeTarget (..),
    ModuleId,
    ModuleName,
    moduleIdUnsafe,
    moduleNameUnsafe,
 )
import Effectful
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import FileSystem.Path (AbsPath (..), ProjectRoot, absPathUnsafe, decodeOsPath, portablePath, relativePathTo)
import Renderable (Renderable (render))
import TypeScript.Config (TsConfig)
import TypeScript.CST (TsNode (..), TsProgram (cst, path))
import TypeScript.ModuleResolver (dropTypeScriptExtension, moduleNames, resolve)

parseAst ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , RoFileSystem :> es
    ) =>
    TsProgram -> Eff es AstModule
parseAst prog = do
    -- Canonicalised through the same call every edge resolves with. Were the
    -- two to disagree - over a symlink, or a macOS /var against /private/var -
    -- one file would become two vertices and every chain through it would die.
    canonical <- fsMkAbsolute prog.path.osPath
    names <- programNames canonical
    nodes <- traverse (edgeOf canonical) prog.cst
    pure
        AstModule
            { id = moduleIdOf canonical
            , names = names
            , path = prog.path
            , nodes = catMaybes nodes
            }
  where
    programNames canonical = do
        root <- ask @ProjectRoot
        aliases <- moduleNames canonical
        pure . fromMaybe (rawPathName root :| []) . nonEmpty $ aliases

    -- Relative to the project root, never to this machine: a name is what a
    -- Rulebook pattern matches and what a Rule Violation's Problem Id is built
    -- from, and both travel to other checkouts. The leading '/' keeps an
    -- unaliased module out of the namespace bare package specifiers live in.
    rawPathName :: ProjectRoot -> ModuleName
    rawPathName root =
        moduleNameUnsafe
            . ("/" <>)
            . portablePath
            . relativePathTo root
            . absPathUnsafe
            . dropTypeScriptExtension
            $ prog.path.osPath

{- | Identity for TypeScript: the canonical path of the file. Not the module's
name, which is what an import writes and what a Rule matches, and of which one
file has several.
-}
moduleIdOf :: AbsPath -> ModuleId
moduleIdOf = moduleIdUnsafe . decodeOsPath . (.osPath)

edgeOf ::
    (Reader TsConfig :> es, RoFileSystem :> es) =>
    AbsPath -> TsNode -> Eff es (Maybe AstNode)
edgeOf importer node = case node of
    Import _ t _ -> Just <$> edge ImportEdge t
    ReExport _ t _ -> Just <$> edge ReExportEdge t
    Source _ -> pure Nothing
  where
    edge kind specifier = do
        let name = moduleNameUnsafe specifier
        resolved <- resolve importer name
        pure
            DependencyEdge
                { specifier = name
                , target = maybe Unresolved (ToModule . moduleIdOf) resolved
                , kind = kind
                , rawStatement = render node
                }
