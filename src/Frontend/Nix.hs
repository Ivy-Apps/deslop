{- | Nix as a 'CompilerFrontend': a project with a @flake.nix@ or a
@default.nix@ at its root, whose files lower to one 'Deslop.Module.Module'
each. A @.nix@ file is read, and depends on every path it writes. Any other
file is never read: 'lower' lists it itself, as the driver reads all that
'discover' returns. See @docs\/adr\/0023@ and @docs\/adr\/0026@.
-}
module Frontend.Nix (
    NixFrontend (..),
    NixUnit (..),
    NixLowerError (..),
    NixConfigError (..),
) where

import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful.Reader.Static (ask)
import Effects.FileSystem (fsFileExists)
import FileSystem.Path (ProjectRoot (..), encodeOsPath, withAbsBaseUnsafe)
import Frontend.Nix.Config (NixConfigError (..), loadNixConfig)
import Frontend.Nix.Ignore (nixEcosystemIgnored)
import Frontend.Nix.Iterator (getNixFiles, getOtherFiles)
import Frontend.Nix.Module (NixLowerError (..), lowerProject)
import Frontend.Nix.Reference (Scope)
import Frontend.Nix.Unit (NixUnit (..), readUnit, renderUnit)
import Git.Ignore (loadGitIgnore)

-- | Witness that the project is a Nix one.
data NixFrontend = NixFrontend
    deriving stock (Show, Eq)

instance CompilerFrontend NixFrontend where
    type Config NixFrontend = Scope
    type ConfigError NixFrontend = NixConfigError
    type Unit NixFrontend = NixUnit
    type LowerError NixFrontend = NixLowerError

    detect projectRoot = bool Nothing (Just NixFrontend) . or <$> traverse marker ["flake.nix", "default.nix"]
      where
        marker = fsFileExists . withAbsBaseUnsafe projectRoot.path . encodeOsPath
    label _ = "Nix"
    lookingFor _ = "Nix (flake.nix or default.nix)"
    codeFence _ = CodeFence "nix"
    builtInRules _ = []
    ecosystemIgnored _ _ = nixEcosystemIgnored
    loadConfig _ = loadNixConfig
    discover _ _ projectRoot gitIgnore = getNixFiles gitIgnore projectRoot
    parse _ _ = readUnit
    render _ = renderUnit
    lint _ _ _ = pure []
    lower _ scope units = do
        projectRoot <- ask @ProjectRoot
        gitIgnore <- loadGitIgnore nixEcosystemIgnored projectRoot.path
        otherFiles <- getOtherFiles gitIgnore projectRoot
        lowerProject projectRoot scope otherFiles units
