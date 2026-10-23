{- | How a language plugs into Deslop: a staged compiler frontend.

Each stage is small and named for what it does, so the language-agnostic
driver, "Deslop.Frontend.Pipeline", can own everything that is the same in
every language - the Command, concurrency, the Baseline, fix write-back and
logging - while a frontend owns only what its language decides. See
@docs/adr/0020@.
-}
module Deslop.Frontend (
    CompilerFrontend (..),
    SomeFrontend (..),
    Supported (..),
    detectFirst,
    lookingForAny,
) where

import Deslop.Error (Warning)
import Deslop.Frontend.Finding (Finding)
import Deslop.Module (Module)
import Deslop.Problem (LintRuleId)
import Deslop.Problem.Formatter (CodeFence)
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (RoFileSystem)
import FileSystem.Iterator (Entry)
import FileSystem.Path (AbsPath, ProjectRoot)
import Git.Ignore (GitIgnore)
import Renderable (Renderable)

{- | One language, from the bytes on disk to the language-agnostic 'Module's
every Rule is judged against.

@fe@ is a witness that the language was detected; every method takes it so
that the associated types below are never ambiguous.

The laws an instance must keep, each stated again on its method:

* 'parse' is total and 'render' is its inverse: @render fe (parse fe cfg p b) == b@
  for every @b@, not only for valid source.
* 'lower' returns a 'Module' for every unit it is given, including those
  'parse' could not read.
* Every module has a canonical name no other module in the project shares.
* Modules inside the project and outside it are named the same way,
  slash-segmented, so one Rule pattern reads both.
* 'lint' never reports, logs or writes, and its rewrites in one unit commute.

@docs\/adr\/0020@ explains why, with an example per language.
-}
class
    ( Renderable (ConfigError fe)
    , Renderable (LowerError fe)
    ) =>
    CompilerFrontend fe
    where
    -- | The project's own configuration, e.g. a resolved @tsconfig.json@.
    type Config fe

    type ConfigError fe

    {- | One parsed file. Lossless: rendering it gives back its bytes. Usually a
    sum with an opaque case holding the bytes of a file that cannot be decoded,
    so that 'parse' stays total.
    -}
    type Unit fe

    {- | What only the whole program shows, and the user should hear about: a
    file whose imports could not be read, two files claiming one module. The
    driver logs each as a warning; it never fails the run. Rendered without the
    @WARNING:@ prefix, which the driver adds.
    -}
    type LowerError fe

    -- | Whether the project at the root is written in this language.
    detect :: (RoFileSystem :> es) => ProjectRoot -> Eff es (Maybe fe)

    -- | The language's name, e.g. @"TypeScript"@.
    label :: fe -> Text

    {- | What 'detect' looks for, named for a person who has none of it, e.g.
    @"TypeScript (tsconfig.json)"@.
    -}
    lookingFor :: Proxy fe -> Text

    -- | The Markdown fence a quoted statement is shown in, e.g. @ts@.
    codeFence :: fe -> CodeFence

    -- | The Lint Rules 'lint' can report, each counted as a Rule the run enforced.
    builtInRules :: fe -> [LintRuleId]

    {- | What the language's own tooling generates and no run should walk -
    dependency folders and build output - on top of what is always ignored.
    The driver hands it to "Git.Ignore.loadGitIgnore", so the 'GitIgnore'
    'discover' is given already honours it.
    -}
    ecosystemIgnored :: fe -> Config fe -> Entry -> Bool

    loadConfig ::
        (RoFileSystem :> es) =>
        fe -> ProjectRoot -> Eff es (Either (ConfigError fe) (Config fe, [Warning]))

    {- | Every source file of the project. A walk prunes whatever
    "Git.Ignore.isIgnored" says the 'GitIgnore' ignores - what is always
    ignored, what 'ecosystemIgnored' names and what is gitignored - without
    descending into it.
    -}
    discover ::
        (RoFileSystem :> es) =>
        fe -> Config fe -> ProjectRoot -> GitIgnore -> Eff es [AbsPath]

    {- | One file's bytes, read as the project's 'Config' says: in some
    languages the configuration decides how a file is read, e.g. a @.cabal@
    file's @default-extensions@ such as @CPP@ or @PackageImports@.

    Total: there is no input it cannot parse. Write it as an island grammar -
    recognise what Deslop reads, an import or a re-export, and keep everything
    else verbatim as raw text - and keep bytes it cannot decode, such as a file
    that is not UTF-8, as one opaque island. A file that failed to parse would
    vanish from 'lower''s module index, and every import of it would resolve to
    something else.
    -}
    parse :: fe -> Config fe -> AbsPath -> ByteString -> Unit fe

    {- | The inverse of 'parse', over every input:

    > render fe (parse fe cfg p b) == b

    What lets @deslop fix@ rewrite one statement and leave every other byte
    alone. Test it with a property over arbitrary bytes.
    -}
    render :: fe -> Unit fe -> ByteString

    {- | What is wrong with one unit and how to repair it. Never reports, logs
    nor writes, and never asks about the Baseline: the driver decides which
    repairs apply. A 'Deslop.Frontend.Finding.Rewrite' changes only what its
    Problem is about, and the rewrites of one unit commute. Every rule id it
    reports is in 'builtInRules'.
    -}
    lint ::
        (RoFileSystem :> es, Reader ProjectRoot :> es) =>
        fe -> Config fe -> Unit fe -> Eff es [Finding (Unit fe)]

    {- | Every unit of the project, lowered at once: a language may need the
    whole program - a module index, a package spanning files - to name one.

    * Every unit is lowered, opaque ones included: a module whose source could
      not be read still exists, with no edges, and a 'LowerError' says so.
    * A module's canonical name, 'Deslop.Module.canonicalName', is unique within
      the project. A Rule Violation's Problem id is built from it, so two
      modules sharing one share every Baseline entry. Qualify it with whatever
      tells them apart - a Haskell executable's @Main@ is
      @exe\/<component>\/Main@ - and keep the language's own name as an alias.
    * Every 'Deslop.Module.ModuleName' is minted here, slash-segmented whatever
      the language writes, for a 'Deslop.Module.Resolved' target and an
      'Deslop.Module.External' one alike: @import Data.Text@ reaches
      @External Data\/Text@, spelled as a project module would be, so a Rule
      is written once for both.
    * A 'Deslop.Module.ModuleId' and the names agree: one module, one id, one
      set of names, wherever an edge reaches it from.
    -}
    lower ::
        (RoFileSystem :> es, Reader ProjectRoot :> es, Concurrent :> es) =>
        fe -> Config fe -> [Unit fe] -> Eff es ([Module], [LowerError fe])

-- | Whichever language was detected.
data SomeFrontend = forall fe. (CompilerFrontend fe) => SomeFrontend fe

-- | A language Deslop can be asked to recognise, before it has recognised one.
data Supported = forall fe. (CompilerFrontend fe) => Supported (Proxy fe)

{- | The first language, in the order given, that recognises the project.

First match wins, so a polyglot project is read as whichever of its languages
comes first.
-}
detectFirst :: (RoFileSystem :> es) => [Supported] -> ProjectRoot -> Eff es (Maybe SomeFrontend)
detectFirst languages projectRoot = runMaybeT . asum . fmap detected $ languages
  where
    detected (Supported proxy) = SomeFrontend <$> MaybeT (detectAs proxy projectRoot)

detectAs ::
    forall fe es.
    (CompilerFrontend fe, RoFileSystem :> es) =>
    Proxy fe ->
    ProjectRoot ->
    Eff es (Maybe fe)
detectAs _ = detect

-- | What each language looks for, for someone whose project matched none.
lookingForAny :: [Supported] -> [Text]
lookingForAny = fmap $ \(Supported proxy) -> lookingFor proxy
