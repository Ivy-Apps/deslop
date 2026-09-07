{- | What Deslop reasons about, once a language frontend has done its work.

Deliberately small, and deliberately free of any one language: an 'AstModule'
is who a module is, what it is called, the file it came from, and the modules
it depends on. That is everything "Deslop.CodeGraph" needs to build a graph and
everything "Deslop.Rule.Enforcer" needs to judge one, so it is everything a new
language has to produce to be supported.

Identity and name are two different things here, and keeping them apart is what
makes the graph correct - see
@docs\/adr\/0016-a-module-is-identified-by-what-it-resolves-to.md@. A module has
exactly one 'ModuleId' and may answer to several 'ModuleName's.

Producing one from TypeScript is "TypeScript.AST".
-}
module Deslop.AST (
    ModuleId (text),
    moduleIdUnsafe,
    ModuleName (text),
    moduleNameUnsafe,
    EdgeTarget (..),
    EdgeKind (..),
    AstNode (..),
    AstModule (..),
    canonicalName,
) where

import Data.List.NonEmpty qualified as NE
import FileSystem.Path (AbsPath)

{- | Who a module is. Opaque: a language frontend mints it and the core never
interprets it, needing only equality and an ordering to key a graph by it.

TypeScript mints it from the canonical path of the file; a Go frontend would
mint it from a package directory, and a Rust one from a crate-qualified module
path. That is why it is not an 'AbsPath': "a module is a file" is true of
TypeScript and false of Go.

It is machine-specific, so it never reaches a report or a baseline - only a
'ModuleName' does.
-}
newtype ModuleId = ModuleId
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)

moduleIdUnsafe :: Text -> ModuleId
moduleIdUnsafe = ModuleId

{- | What a module is called - e.g. @\@\/lib\/util@. The vocabulary Rulebook
Rules and reports are written in.

A module has more than one when more than one name resolves to it: a barrel
answers to both @\@\/features\/home@ and @\@\/features\/home\/index@, and every
alias mapping to the same file adds another. A pattern matching /any/ of them
matches the module.
-}
newtype ModuleName = ModuleName
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)

moduleNameUnsafe :: Text -> ModuleName
moduleNameUnsafe = ModuleName

{- | What a dependency points at. The frontend resolves the written specifier
before the core sees it, so the core never has to guess whether a dependency is
one of ours.
-}
data EdgeTarget
    = -- | Resolved to a module the frontend recognises.
      ToModule ModuleId
    | -- | @react@, or a specifier that names nothing on disk.
      Unresolved
    deriving stock (Show, Eq, Ord)

{- | Whether a dependency is private to the module or part of the surface it
exposes. 'ReExportEdge' is TypeScript's @export ... from@, Rust's @pub use@ and
a Haskell export list; Go and Kotlin have no such construct and mint only
'ImportEdge'.

The graph does not care - both are edges. Reports do, because a sentence saying
"imports" above a quoted @export@ statement is a sentence a reader stops
trusting.
-}
data EdgeKind
    = ImportEdge
    | ReExportEdge
    deriving stock (Show, Eq, Ord)

-- | One dependency of a module, as written and as resolved.
data AstNode = DependencyEdge
    { specifier :: ModuleName
    -- ^ What the source actually wrote. A fact of the file, so it is here on
    -- every edge rather than on one branch of 'EdgeTarget'.
    , target :: EdgeTarget
    , kind :: EdgeKind
    , rawStatement :: Text
    -- ^ Verbatim, so a report can quote it back.
    }
    deriving stock (Show, Eq)

data AstModule = AstModule
    { id :: ModuleId
    -- ^ Identity.
    , names :: NonEmpty ModuleName
    -- ^ Canonical name first; see 'canonicalName'.
    , path :: AbsPath
    -- ^ An attribute, not an identity: where a report points the reader.
    , nodes :: [AstNode]
    }
    deriving stock (Show, Eq)

{- | The name a report prints and a baseline id is built from. Fixed per module
so that a Problem keeps the same id from run to run.
-}
canonicalName :: AstModule -> ModuleName
canonicalName = NE.head . (.names)
