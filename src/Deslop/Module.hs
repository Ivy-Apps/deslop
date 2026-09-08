{- | What Deslop reasons about, once a language frontend has done its work.

Deliberately small, and deliberately free of any one language: a 'Module' is who
a module is, what it is called, the file it came from, and the modules it
depends on. That is everything "Deslop.CodeGraph" needs to build a graph and
everything "Deslop.Rule.Enforcer" needs to judge one, so it is everything a new
language has to produce to be supported.

Nothing here is a tree, which is why this module is not called an AST. A
frontend hands over a flat list of modules and the edges between them; parsing
is its own business and ends before this seam.

Four nouns that are easy to confuse, and the whole design rests on keeping them
apart - see
@docs\/adr\/0017-a-module-is-identified-by-what-it-resolves-to.md@:

* a 'ModuleId' says /which/ module this is. Exactly one per module, and never
  rendered.
* a 'ModuleName' says what it is /called/. Several per module, and what
  Rulebook Rules and reports are written in.
* a 'Specifier' is what an author /typed/. A fact of one file, and not a name:
  @.\/helper@ names nothing, it points somewhere from where its writer stood.
* a 'Module' is the thing itself.

Producing one from TypeScript is "TypeScript.Module".
-}
module Deslop.Module (
    ModuleId (text),
    moduleIdUnsafe,
    ModuleName (text),
    moduleNameUnsafe,
    Specifier (text),
    specifierUnsafe,
    Location (..),
    EdgeTarget (..),
    EdgeKind (..),
    DependencyEdge (..),
    Module (..),
    canonicalName,
) where

import Data.List.NonEmpty qualified as NE
import FileSystem.Path (ProjectRelativePath)

{- | Who a module is. Opaque: a language frontend mints it and the core never
interprets it, needing only equality and an ordering to key a graph by it.

TypeScript mints it from the canonical path of the file; a Go frontend would
mint it from a package directory, and a Rust one from a crate-qualified module
path. That is why it is not a path: "a module is a file" is true of TypeScript
and false of Go.

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

Always spelled with @\/@, whatever the language writes - see
@docs\/adr\/0019-module-names-are-slash-segmented-in-every-language.md@. A
Haskell frontend mints @Data\/List\/NonEmpty@, not @Data.List.NonEmpty@.
-}
newtype ModuleName = ModuleName
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)

moduleNameUnsafe :: Text -> ModuleName
moduleNameUnsafe = ModuleName

{- | The text a source file writes to name what it depends on - @\"\@\/a\"@,
@\".\/helper\"@, @\"react\"@. What the author typed, before anything resolved
it.

Not a 'ModuleName', however alike the two often look. @.\/helper@ is a perfectly
good specifier and can never be a name, because it means "next to me" and so
denotes a different module for every file that writes it. A name means the same
thing to every reader.

The two answer different questions, which is why an edge carries both:
"TypeScript.Lint.RelativeSpecifiers" judges what was /written/, and a Rule's
@forbids@ judges what was /reached/.
-}
newtype Specifier = Specifier
    { text :: Text
    }
    deriving stock (Show, Eq, Ord)

specifierUnsafe :: Text -> Specifier
specifierUnsafe = Specifier

{- | Where in the source something is, and what the source says there.

One type for both halves because a report wants both together and never one
without the other: a line with nothing quoted makes the reader open the file,
and a quote with no line makes them search it.

@line@ is for the reader only. It must never enter a 'Deslop.Problem.ProblemId'
- an id that moved when someone added a blank line above it would unsuppress
every Baseline entry below.
-}
data Location = Location
    { file :: ProjectRelativePath
    , line :: Int
    -- ^ 1-based.
    , code :: Text
    -- ^ Verbatim, so a report can quote it back.
    }
    deriving stock (Eq, Show, Ord)

{- | What a dependency points at. The frontend resolves the written specifier
before the core sees it, so the core never has to guess whether a dependency is
one of ours - nor what to call it, which is the most language-specific question
there is.

'Resolved' carries the names because only the frontend can produce them: a Go
frontend answers "one import path" and a Rust one "the crate path plus every
@pub use@ alias". The core would have to invent them, and the only material it
could invent them from is the specifier, which is not a name.
-}
data EdgeTarget
    = -- | Resolved to a module the frontend recognises, and every name that
      -- module answers to, canonical first. The module may still be one
      -- nobody parsed - gitignored, or outside the scanned tree - which is
      -- why the names travel with the edge rather than being looked up.
      Resolved ModuleId (NonEmpty ModuleName)
    | -- | @react@, or a specifier that names nothing the frontend can resolve.
      External Specifier
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
data DependencyEdge = DependencyEdge
    { specifier :: Specifier
    -- ^ What the source actually wrote. A fact of the file, so it is here on
    -- every edge rather than on one branch of 'EdgeTarget'.
    , target :: EdgeTarget
    , kind :: EdgeKind
    , location :: Location
    -- ^ Where the dependency was written, and the statement itself.
    }
    deriving stock (Show, Eq)

data Module = Module
    { id :: ModuleId
    -- ^ Identity.
    , names :: NonEmpty ModuleName
    -- ^ Canonical name first; see 'canonicalName'.
    , path :: ProjectRelativePath
    -- ^ An attribute, not an identity: where a report points the reader. A
    -- file in TypeScript, and a directory in a language whose module is a
    -- package.
    , edges :: [DependencyEdge]
    }
    deriving stock (Show, Eq)

{- | The name a report prints and a baseline id is built from. Fixed per module
so that a Problem keeps the same id from run to run.
-}
canonicalName :: Module -> ModuleName
canonicalName = NE.head . (.names)
