{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE: Python as a 'CompilerFrontend'. Throwaway exploration of ADR 20.

A project with a @pyproject.toml@, @setup.py@, @setup.cfg@ or
@requirements.txt@ at its root, whose @.py@ files lower to one
'Deslop.Module.Module' each, named by their dotted import path.
-}
module Frontend.Python (
    PyFrontend (..),
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Frontend (CompilerFrontend (..))
import Deslop.Problem.Formatter (CodeFence (..))
import Effects.FileSystem (fsFileExists)
import FileSystem.Iterator (Entry (..), walkDir)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, withAbsBaseUnsafe)
import Frontend.Python.Config (PyConfig, loadPyConfig)
import Frontend.Python.Lint (noRelativeImports, pythonRules)
import Frontend.Python.Module (lowerPy)
import Frontend.Python.Parser (PyParseError, PyUnit, parsePy, renderPy)
import Git.Ignore (alwaysIgnored, isIgnored)
import System.OsPath (OsPath, osp, takeExtension, takeFileName)

-- | Witness that the project is a Python one.
data PyFrontend = PyFrontend
    deriving stock (Show, Eq)

instance CompilerFrontend PyFrontend where
    type Config PyFrontend = PyConfig
    type Unit PyFrontend = PyUnit
    type ParseError PyFrontend = PyParseError

    detect projectRoot =
        bool Nothing (Just PyFrontend) . or
            <$> traverse (fsFileExists . withAbsBaseUnsafe projectRoot.path) markers
    label _ = "Python"
    lookingFor _ = "Python (pyproject.toml, setup.py, setup.cfg or requirements.txt)"
    codeFence _ = CodeFence "python"
    builtInRules _ = pythonRules
    ecosystemIgnored _ = pyEcosystemIgnored
    loadConfig _ = loadPyConfig
    discover _ _ projectRoot gitIgnore = walkDir skip pyFile projectRoot.path
      where
        skip e = alwaysIgnored e || pyEcosystemIgnored e || isIgnored gitIgnore e
        pyFile e = e.path <$ guard (not e.isDir && takeExtension e.path.osPath == [osp|.py|])
    parse _ = parsePy
    render _ = renderPy
    lint _ = noRelativeImports
    lower _ = lowerPy

markers :: [OsPath]
markers = [[osp|pyproject.toml|], [osp|setup.py|], [osp|setup.cfg|], [osp|requirements.txt|]]

{- | What Python tooling generates. Matched by name only: a virtualenv called
anything else (@env@, @py311@) is walked, because recognising one means looking
for the @pyvenv.cfg@ inside it, and this predicate cannot read the filesystem.
-}
pyEcosystemIgnored :: Entry -> Bool
pyEcosystemIgnored e = name `Set.member` names || ".egg-info" `T.isSuffixOf` name
  where
    name = decodeOsPath . takeFileName $ e.path.osPath
    names =
        Set.fromList
            [ ".venv"
            , "venv"
            , "__pycache__"
            , "site-packages"
            , ".tox"
            , ".nox"
            , ".mypy_cache"
            , ".pytest_cache"
            , ".ruff_cache"
            , ".eggs"
            , "build"
            , "dist"
            ]
