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
-}
class (Renderable (ParseError fe), Renderable (ConfigError fe)) => CompilerFrontend fe where
    -- | The project's own configuration, e.g. a resolved @tsconfig.json@.
    type Config fe

    type ConfigError fe

    -- | One parsed file. Lossless: rendering it gives back its bytes.
    type Unit fe

    {- | Why a file could not be parsed, rendered for the person reading it.
    The driver prefixes the file's project-relative path, so the rendered
    error should not repeat it: a position within the file is enough.
    -}
    type ParseError fe

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
    ecosystemIgnored :: fe -> Entry -> Bool

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
    -}
    parse :: fe -> Config fe -> AbsPath -> ByteString -> Either (ParseError fe) (Unit fe)

    -- | The inverse of 'parse': @render fe <$> parse fe cfg p b == Right b@.
    render :: fe -> Unit fe -> ByteString

    -- | What is wrong with one unit and how to repair it. Never reports nor writes.
    lint ::
        (RoFileSystem :> es, Reader ProjectRoot :> es) =>
        fe -> Config fe -> Unit fe -> Eff es [Finding (Unit fe)]

    {- | Every unit of the project, lowered at once: a language may need the
    whole program - a module index, a package spanning files - to name one.
    -}
    lower ::
        (RoFileSystem :> es, Reader ProjectRoot :> es, Concurrent :> es) =>
        fe -> Config fe -> [Unit fe] -> Eff es [Module]

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
