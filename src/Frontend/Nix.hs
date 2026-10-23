{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE. Nix as a 'CompilerFrontend': a project with a @flake.nix@ (or, for
a pre-flake project, a @default.nix@ or @shell.nix@) at its root, whose @.nix@
files lower to one 'Deslop.Module.Module' each.
-}
module Frontend.Nix (
    NixFrontend (..),
    NixLayout (..),
) where

import Data.Text qualified as T
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Frontend.Finding (Finding, fixable, unfixable)
import Deslop.Module (Location (..))
import Deslop.Problem (LintRuleId (..), Problem (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effectful (Eff, type (:>))
import Effectful.Concurrent.Async (pooledMapConcurrentlyN)
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (fsFileExists)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, relativePathTo, withAbsBaseUnsafe)
import Frontend.Nix.Module (lowerNix)
import Frontend.Nix.Parser (NixParseError, NixRef (..), NixUnit (..), parseNix, renderNix, rewriteLine)
import Git.Ignore (alwaysIgnored, isIgnored)
import System.OsPath (OsPath, osp, takeExtension, takeFileName)

-- | Witness that the project is a Nix one.
data NixFrontend = NixFrontend
    deriving stock (Show, Eq)

-- | Which marker the root carries.
data NixLayout = Flake | Classic
    deriving stock (Show, Eq)

instance CompilerFrontend NixFrontend where
    type Config NixFrontend = NixLayout
    type Unit NixFrontend = NixUnit
    type ParseError NixFrontend = NixParseError

    detect projectRoot =
        bool Nothing (Just NixFrontend) . or <$> traverse (fsFileExists . at projectRoot) markers
    label _ = "Nix"
    lookingFor _ = "Nix (flake.nix, default.nix or shell.nix)"
    codeFence _ = CodeFence "nix"
    builtInRules _ = [dynamicImportRule, explicitDefaultRule]
    ecosystemIgnored _ = nixEcosystemIgnored
    loadConfig _ projectRoot = bool Classic Flake <$> fsFileExists (at projectRoot [osp|flake.nix|])
    discover _ _ projectRoot gitIgnore = walkDir skip nixFile projectRoot.path
      where
        skip entry = alwaysIgnored entry || nixEcosystemIgnored entry || isIgnored gitIgnore entry
        nixFile entry = entry.path <$ guard (not entry.isDir && takeExtension entry.path.osPath == [osp|.nix|])
    parse _ = parseNix
    render _ = renderNix
    lint _ _ = lintNix
    lower _ _ = pooledMapConcurrentlyN 32 lowerNix

markers :: [OsPath]
markers = [[osp|flake.nix|], [osp|default.nix|], [osp|shell.nix|]]

at :: ProjectRoot -> OsPath -> AbsPath
at projectRoot = withAbsBaseUnsafe projectRoot.path

-- | @nix build@'s out-links: @result@, @result-dev@, @result-doc@...
nixEcosystemIgnored :: Entry -> Bool
nixEcosystemIgnored entry = name == "result" || "result-" `T.isPrefixOf` name
  where
    name = decodeOsPath . takeFileName $ entry.path.osPath

dynamicImportRule :: LintRuleId
dynamicImportRule = LintRuleId "nix-no-dynamic-imports"

explicitDefaultRule :: LintRuleId
explicitDefaultRule = LintRuleId "nix-no-explicit-default"

lintNix :: (Reader ProjectRoot :> es) => NixUnit -> Eff es [Finding NixUnit]
lintNix unit = do
    projectRoot <- ask @ProjectRoot
    let file = relativePathTo projectRoot unit.path
    pure . mapMaybe (findingOf file) $ unit.refs
  where
    findingOf file = \case
        DynamicRef {line, code} ->
            Just . unfixable $
                problem dynamicImportRule file line code
                    "imports a path computed at evaluation time, so Deslop cannot see what it depends on."
                    "List the files explicitly."
        PathRef {line, written, code}
            | Just dir <- T.stripSuffix "/default.nix" written ->
                Just $
                    fixable
                        ( problem explicitDefaultRule file line code
                            "names default.nix, which importing the directory already means."
                            ("Use ```" <> dir <> "``` instead.")
                        )
                        (Endo $ rewriteLine line written dir)
        _ -> Nothing
    problem rule file line code description howTo autoFixable =
        LintProblem
            { lintRule = rule
            , location = Location {file = file, line = line, code = code}
            , description = description
            , fix = howTo
            , autoFixable
            }
