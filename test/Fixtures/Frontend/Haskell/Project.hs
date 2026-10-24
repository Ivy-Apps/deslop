{- | A Haskell project held in memory, run through every stage of the Haskell
frontend the way the driver runs them: load the @.cabal@ file, discover,
parse, lower.
-}
module Fixtures.Frontend.Haskell.Project (
    projectRoot,
    inProject,
    Lowered (..),
    lowerFiles,
    cabalPackage,
    configFrom,
    edgesFrom,
    moduleNamed,
) where

import Data.Text qualified as T
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Module (DependencyEdge (..), EdgeTarget (..), Module (..), ModuleName (..), Specifier (..))
import Doubles.FileSystem (newInMemoryFileSystem, runInMemoryFileSystem)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static (runReader)
import Effects.FileSystem (fsReadFile)
import FileSystem.Path (AbsPath, ProjectRoot (..))
import Frontend.Haskell (HsConfigError, HsFrontend (..), HsLowerError)
import Frontend.Haskell.Cabal (HsConfig, loadHsConfig)
import Git.Ignore (loadGitIgnore)
import TestUtils (ap, requireRight)

projectRoot :: ProjectRoot
projectRoot = ProjectRoot (ap "/project")

inProject :: Text -> AbsPath
inProject path = ap ("/project/" <> path)

data Lowered = Lowered
    { modules :: [Module]
    , warnings :: [HsLowerError]
    }
    deriving stock (Show)

-- | These files, by their path from the project root, through every stage.
lowerFiles :: [(Text, ByteString)] -> IO (Either HsConfigError Lowered)
lowerFiles files = do
    fs <- newInMemoryFileSystem [(inProject path, bytes) | (path, bytes) <- files]
    runEff . runConcurrent . runInMemoryFileSystem fs . runReader projectRoot $
        loadConfig HsFrontend projectRoot >>= \case
            Left err -> pure (Left err)
            Right (cfg, _) -> do
                gitIgnore <- loadGitIgnore (ecosystemIgnored HsFrontend cfg) projectRoot.path
                paths <- discover HsFrontend cfg projectRoot gitIgnore
                units <- traverse (\path -> parse HsFrontend cfg path <$> fsReadFile path) paths
                Right . uncurry Lowered <$> lower HsFrontend cfg units

-- | The @.cabal@ file of a package @p@ holding these stanzas.
cabalPackage :: [Text] -> Text
cabalPackage stanzas = T.unlines (["cabal-version: 3.0", "name: p", "version: 0"] <> stanzas)

-- | The configuration a @p.cabal@ holding this text loads to; the test fails when it does not load.
configFrom :: Text -> IO HsConfig
configFrom cabal = do
    fs <- newInMemoryFileSystem [(inProject "p.cabal", encodeUtf8 cabal)]
    fmap fst . requireRight show =<< runEff (runInMemoryFileSystem fs (loadHsConfig projectRoot))

-- | The module whose canonical name this is.
moduleNamed :: Text -> Lowered -> Maybe Module
moduleNamed name = find ((== name) . (.text) . head . (.names)) . (.modules)

{- | A module's edges as what was written, what it reached - by canonical name,
or @External:@ name - and how.
-}
edgesFrom :: Text -> Lowered -> [(Text, Text, Text)]
edgesFrom name lowered = maybe [] (fmap summary . (.edges)) (moduleNamed name lowered)
  where
    summary edge = (edge.specifier.text, targetName edge.target, T.pack (show edge.kind))
    targetName = \case
        Resolved _ names -> (head names).text
        External n -> "External:" <> n.text
