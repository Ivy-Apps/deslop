{-# LANGUAGE UndecidableInstances #-}

{- | A project run through every stage of a 'CompilerFrontend' the way the
driver runs them: load the configuration, discover, parse, lower. Shared by
the specs of every language, so none of them strings the stages together by
hand.
-}
module Fixtures.Deslop.Frontend (
    Lowered (..),
    lowerProject,
    lowerProjectAt,
    moduleNamed,
    edgesFrom,
    edgeTargetsFrom,
) where

import Data.Text qualified as T
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Module (DependencyEdge (..), EdgeTarget (..), Module (..), ModuleName (..), Specifier (..), canonicalName)
import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (Eff, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Reader.Static (Reader, runReader)
import Effects.FileSystem (RoFileSystem, fsReadFile)
import FileSystem.Path (ProjectRoot (..))
import Fixtures.FileSystem.Path (projectFileSystem, projectRoot)
import Git.Ignore (loadGitIgnore)

-- | Every module of the project, and what lowering it had to say.
data Lowered fe = Lowered
    { modules :: [Module]
    , warnings :: [LowerError fe]
    }

deriving stock instance (Show (LowerError fe)) => Show (Lowered fe)

-- | These files, by their path from the in-memory project root, through every stage.
lowerProject :: (CompilerFrontend fe) => fe -> [(Text, ByteString)] -> IO (Either (ConfigError fe) (Lowered fe))
lowerProject fe files = do
    fs <- projectFileSystem files
    runEff . runConcurrent . runInMemoryFileSystem fs . runReader projectRoot $ lowerProjectAt fe projectRoot

-- | The project at this root, on whatever file system the caller runs, through every stage.
lowerProjectAt ::
    (CompilerFrontend fe, Reader ProjectRoot :> es, RoFileSystem :> es, Concurrent :> es) =>
    fe -> ProjectRoot -> Eff es (Either (ConfigError fe) (Lowered fe))
lowerProjectAt fe root =
    loadConfig fe root >>= \case
        Left err -> pure (Left err)
        Right (cfg, _) -> do
            gitIgnore <- loadGitIgnore (ecosystemIgnored fe cfg) root.path
            paths <- discover fe cfg root gitIgnore
            units <- traverse (\path -> parse fe cfg path <$> fsReadFile path) paths
            Right . uncurry Lowered <$> lower fe cfg units

-- | The module whose canonical name this is.
moduleNamed :: Text -> Lowered fe -> Maybe Module
moduleNamed name = find ((== name) . (.text) . canonicalName) . (.modules)

{- | A module's edges as what was written, what it reached - by canonical name,
@External:@ name or @Unresolved:@ spelling - and how.
-}
edgesFrom :: Text -> Lowered fe -> [(Text, Text, Text)]
edgesFrom name = fmap (\edge -> (edge.specifier.text, targetName edge.target, T.pack (show edge.kind))) . edgesOf name

-- | As 'edgesFrom', for a language whose every edge is an import.
edgeTargetsFrom :: Text -> Lowered fe -> [(Text, Text)]
edgeTargetsFrom name = fmap (\edge -> (edge.specifier.text, targetName edge.target)) . edgesOf name

edgesOf :: Text -> Lowered fe -> [DependencyEdge]
edgesOf name = maybe [] (.edges) . moduleNamed name

targetName :: EdgeTarget -> Text
targetName = \case
    Resolved _ names -> (head names).text
    External n -> "External:" <> n.text
    Unresolved s -> "Unresolved:" <> s.text
