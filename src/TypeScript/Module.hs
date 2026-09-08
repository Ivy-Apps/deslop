{- | Lowering a TypeScript concrete syntax tree into the language-agnostic
"Deslop.Module".

This is the seam a language crosses: everything above it reasons about
'Module's and knows nothing of TypeScript, and everything below it is
TypeScript all the way down to the bytes. A second language earns its support
by supplying a module of this shape and nothing more.

It is also the only place a specifier is resolved. That matters: while the graph
took its edges from whatever the lint pass left in the CST, baselining a lint
Problem silently changed the graph. Resolving here makes the two independent.

Naming what a specifier resolved to happens here too, and for the same reason
it has to: how an import path becomes a module's name is the most
language-specific question there is. A Go frontend answers "one import path", a
Rust one "the crate path plus every @pub use@ alias", and neither answer is one
the core could have reached on its own.
-}
module TypeScript.Module (parseModule) where

import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleId,
    ModuleName,
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import Effectful
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import FileSystem.Path (
    AbsPath (..),
    ProjectRelativePath,
    ProjectRoot,
    absPathUnsafe,
    decodeOsPath,
    portablePath,
    relativePathTo,
 )
import Renderable (Renderable (render))
import TypeScript.Config (TsConfig)
import TypeScript.CST (TsNode (..), TsProgram (cst, path), onLines)
import TypeScript.ModuleResolver (dropTypeScriptExtension, moduleNames, resolve)

parseModule ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , RoFileSystem :> es
    ) =>
    TsProgram -> Eff es Module
parseModule prog = do
    -- Canonicalised through the same call every edge resolves with. Were the
    -- two to disagree - over a symlink, or a macOS /var against /private/var -
    -- one file would become two vertices and every chain through it would die.
    canonical <- fsMkAbsolute prog.path.osPath
    root <- ask @ProjectRoot
    let relPath = relativePathTo root prog.path
    names <- namesFor canonical
    edges <- traverse (edgeOf canonical relPath) (onLines prog.cst)
    pure
        Module
            { id = moduleIdOf canonical
            , names = names
            , path = relPath
            , edges = catMaybes edges
            }

{- | Every name a file answers to, canonical first.

Used for the module being lowered and for whatever each of its edges resolved
to, so that the two can never disagree about what to call one file.
-}
namesFor ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    ) =>
    AbsPath -> Eff es (NonEmpty ModuleName)
namesFor absPath = do
    root <- ask @ProjectRoot
    aliases <- moduleNames absPath
    pure . fromMaybe (rawPathName root :| []) . nonEmpty $ aliases
  where
    -- Spelled from the project root, never from this machine: a name is what a
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
            $ absPath.osPath

{- | Identity for TypeScript: the canonical path of the file. Not the module's
name, which is what an import writes and what a Rule matches, and of which one
file has several.
-}
moduleIdOf :: AbsPath -> ModuleId
moduleIdOf = moduleIdUnsafe . decodeOsPath . (.osPath)

edgeOf ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , RoFileSystem :> es
    ) =>
    AbsPath -> ProjectRelativePath -> (Int, TsNode) -> Eff es (Maybe DependencyEdge)
edgeOf importer file (line, node) = case node of
    Import _ t _ -> Just <$> edge ImportEdge t
    ReExport _ t _ -> Just <$> edge ReExportEdge t
    Source _ -> pure Nothing
  where
    edge kind written = do
        let specifier = specifierUnsafe written
        target <- targetOf specifier
        pure
            DependencyEdge
                { specifier = specifier
                , target = target
                , kind = kind
                , location = Location {file = file, line = line, code = render node}
                }

    targetOf specifier =
        resolve importer specifier >>= \case
            Nothing -> pure . External $ specifier
            Just resolved -> Resolved (moduleIdOf resolved) <$> namesFor resolved
