{- | Haskell as a 'CompilerFrontend': a project with one @*.cabal@ file at its
root, whose components' @.hs@ files lower to one 'Deslop.Module.Module' each.
See @docs\/adr\/0021@.
-}
module Frontend.Haskell (
    HsFrontend (..),
    HsUnit (..),
    HsLowerError (..),
    HsConfigError,
) where

import Data.Text qualified as T
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful.Reader.Static (ask)
import FileSystem.Path (ProjectRoot (..), portablePath, relativePathTo)
import Frontend.Haskell.Cabal (CabalError (..), HsConfig, cabalFilesIn, loadHsConfig)
import Frontend.Haskell.Ignore (hsEcosystemIgnored)
import Frontend.Haskell.Iterator (getHsFiles)
import Frontend.Haskell.Module (HsLowerError (..), lowerProject)
import Frontend.Haskell.Unit (HsUnit (..), readUnit, renderUnit)
import Renderable (Renderable)
import Renderable qualified

-- | Witness that the project is a Haskell one.
data HsFrontend = HsFrontend
    deriving stock (Show, Eq)

data HsConfigError = HsConfigError !ProjectRoot !CabalError
    deriving stock (Show, Eq)

instance Renderable HsConfigError where
    render (HsConfigError projectRoot err) = case err of
        NotOneCabalFile paths ->
            "Deslop reads a single Haskell package, with one .cabal file at the project root, but found "
                <> show (length paths)
                <> ": "
                <> T.intercalate ", " (fmap relative paths)
                <> "."
        CabalUnreadable path errors ->
            "Failed to read " <> relative path <> ":\n" <> T.intercalate "\n" errors
        CabalProjectUnreadable path reason ->
            "Failed to read " <> relative path <> ": " <> reason
        MultiPackageProject path packages ->
            relative path
                <> " names packages other than the one at the root ("
                <> T.intercalate ", " packages
                <> "). Deslop reads a single Haskell package; run it on each package's directory instead."
      where
        relative = portablePath . relativePathTo projectRoot

instance CompilerFrontend HsFrontend where
    type Config HsFrontend = HsConfig
    type ConfigError HsFrontend = HsConfigError
    type Unit HsFrontend = HsUnit
    type LowerError HsFrontend = HsLowerError

    -- Any .cabal file at the root, not parsed: 'loadConfig' says what is
    -- wrong with it, two of them included.
    detect projectRoot = bool Nothing (Just HsFrontend) . not . null <$> cabalFilesIn projectRoot.path
    label _ = "Haskell"
    lookingFor _ = "Haskell (*.cabal)"
    codeFence _ = CodeFence "haskell"
    builtInRules _ = []
    ecosystemIgnored _ _ = hsEcosystemIgnored
    loadConfig _ projectRoot = first (HsConfigError projectRoot) <$> loadHsConfig projectRoot
    discover _ cfg _ gitIgnore = getHsFiles gitIgnore cfg
    parse _ = readUnit
    render _ = renderUnit
    lint _ _ _ = pure []
    lower _ cfg units = do
        projectRoot <- ask @ProjectRoot
        lowerProject projectRoot cfg units
