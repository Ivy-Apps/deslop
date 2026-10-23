{-# LANGUAGE QuasiQuotes #-}

{- | SPIKE: where a Python project's import roots are, and what a file is called.

A real loader would read @pyproject.toml@ (setuptools @packages.find.where@,
Poetry @packages@, Hatch @sources@, uv workspace members), @setup.cfg@
@package_dir@, and could never know what @setup.py@ computes. This one guesses:
@src/@ when it exists, then the project root.
-}
module Frontend.Python.Config (
    PyConfig (..),
    loadPyConfig,
    qualifiedName,
    isPackageInit,
) where

import Data.Text qualified as T
import Effectful (Eff, type (:>))
import Effects.FileSystem (RoFileSystem, fsDirectoryExists)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, withAbsBaseUnsafe)
import System.OsPath (osp, splitDirectories, takeFileName)

-- | The directories on @sys.path@, deepest first so a file picks the nearest.
newtype PyConfig = PyConfig
    { sourceRoots :: [AbsPath]
    }

loadPyConfig :: (RoFileSystem :> es) => ProjectRoot -> Eff es PyConfig
loadPyConfig projectRoot = do
    let src = withAbsBaseUnsafe projectRoot.path [osp|src|]
    hasSrc <- fsDirectoryExists src
    pure . PyConfig $ [src | hasSrc] <> [projectRoot.path]

{- | The dotted name a file is imported by, from the nearest source root:
@src/tiny/core.py@ is @tiny.core@ and @src/tiny/__init__.py@ is @tiny@.
-}
qualifiedName :: PyConfig -> AbsPath -> [Text]
qualifiedName cfg file = case mapMaybe below cfg.sourceRoots of
    segs : _ -> moduleSegs segs
    [] -> moduleSegs . fmap decodeOsPath . splitDirectories $ file.osPath
  where
    below root = fmap decodeOsPath <$> stripPrefixList (splitDirectories root.osPath) (splitDirectories file.osPath)
    stripPrefixList (x : xs) (y : ys) | x == y = stripPrefixList xs ys
    stripPrefixList [] ys = Just ys
    stripPrefixList _ _ = Nothing
    moduleSegs segs = case viaNonEmpty last segs of
        Just "__init__.py" -> fromMaybe ["__init__"] . nonEmpty' . take (length segs - 1) $ segs
        Just f -> take (length segs - 1) segs <> [fromMaybe f . T.stripSuffix ".py" $ f]
        Nothing -> []
    nonEmpty' xs = xs <$ guard (not (null xs))

isPackageInit :: AbsPath -> Bool
isPackageInit p = takeFileName p.osPath == [osp|__init__.py|]
