{- | A Haskell project held in memory, run through every stage of the Haskell
frontend the way the driver runs them (see "Fixtures.Deslop.Frontend").
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
import Doubles.FileSystem (runInMemoryFileSystem)
import Effectful (runEff)
import Fixtures.Deslop.Frontend (Lowered (..), edgesFrom, lowerProject, moduleNamed)
import Fixtures.FileSystem.Path (inProject, projectFileSystem, projectRoot)
import Frontend.Haskell (HsConfigError, HsFrontend (..))
import Frontend.Haskell.Cabal (HsConfig, loadHsConfig)
import TestUtils (requireRight)

-- | These files, by their path from the project root, through every stage.
lowerFiles :: [(Text, ByteString)] -> IO (Either HsConfigError (Lowered HsFrontend))
lowerFiles = lowerProject HsFrontend

-- | The @.cabal@ file of a package @p@ holding these stanzas.
cabalPackage :: [Text] -> Text
cabalPackage stanzas = T.unlines (["cabal-version: 3.0", "name: p", "version: 0"] <> stanzas)

-- | The configuration a @p.cabal@ holding this text loads to; the test fails when it does not load.
configFrom :: Text -> IO HsConfig
configFrom cabal = do
    fs <- projectFileSystem [("p.cabal", encodeUtf8 cabal)]
    fmap fst . requireRight show =<< runEff (runInMemoryFileSystem fs (loadHsConfig projectRoot))
