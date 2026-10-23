{- | SPIKE: lowering every Python unit at once.

The module index - which dotted name each discovered file answers to - is what
decides @from pkg import name@: an edge to @pkg/name@ when that is a submodule,
to @pkg@ when @name@ is an attribute of it. Only units that parsed are in the
index, so a submodule whose file failed to parse is silently taken for an
attribute of its package.
-}
module Frontend.Python.Module (
    lowerPy,
    absoluteBase,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deslop.Module (
    DependencyEdge (..),
    EdgeKind (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleId,
    ModuleName,
    moduleIdUnsafe,
    moduleNameUnsafe,
    specifierUnsafe,
 )
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask)
import FileSystem.Path (ProjectRoot, relativePathTo)
import Frontend.Python.Config (PyConfig, isPackageInit, qualifiedName)
import Frontend.Python.Parser (PyImport (..), PyLine (..), PyUnit (..), importedNames)

type Index = Map [Text] ()

lowerPy :: (Reader ProjectRoot :> es) => PyConfig -> [PyUnit] -> Eff es [Module]
lowerPy cfg units = do
    root <- ask @ProjectRoot
    pure . fmap (lowerOne root) $ units
  where
    index :: Index
    index = Map.fromList . fmap ((,()) . qualifiedName cfg . (.path)) $ units

    lowerOne root unit =
        let self = qualifiedName cfg unit.path
            file = relativePathTo root unit.path
            isInit = isPackageInit unit.path
         in Module
                { id = idOf self
                , names = nameOf self :| []
                , path = file
                , edges = concat . zipWith (edgesAt file self isInit) [1 ..] $ unit.lines
                }

    edgesAt file self isInit lineNo line = case line.stmt of
        Nothing -> []
        Just imp ->
            let loc = Location {file = file, line = lineNo, code = T.dropWhileEnd (== '\r') . decodeUtf8 $ line.raw}
             in edgesOf self isInit loc imp

    edgesOf _ _ loc (ImportModules mods) =
        [ DependencyEdge
            { specifier = specifierUnsafe m
            , target = targetOf (T.splitOn "." m)
            , kind = ImportEdge
            , location = loc
            }
        | m <- mods
        , not (T.null m)
        ]
    edgesOf self isInit loc imp@FromImport {} =
        ordNubOn (.target) $ fmap edgeFor (importedNames imp.afterImport)
      where
        written = T.replicate imp.level "." <> T.intercalate "." imp.modSegs
        base = absoluteBase self isInit imp.level imp.modSegs
        kind = bool ImportEdge ReExportEdge (isInit && imp.level > 0)
        edgeFor name = case base of
            Nothing -> DependencyEdge (specifierUnsafe written) (External (specifierUnsafe written)) kind loc
            Just b
                | Map.member (b <> [name]) index ->
                    DependencyEdge (specifierUnsafe (written <> sep <> name)) (resolved (b <> [name])) kind loc
                | otherwise -> DependencyEdge (specifierUnsafe written) (targetOf b) kind loc
        sep = bool "." "" (T.null written || T.isSuffixOf "." written)

    targetOf segs
        | Map.member segs index = resolved segs
        | otherwise = External . specifierUnsafe . T.intercalate "." $ segs
    resolved segs = Resolved (idOf segs) (nameOf segs :| [])

{- | The absolute dotted module a @from@ names, from the importing module:
@from ..models import User@ in @tiny.sub.mod@ is @tiny.models@.
-}
absoluteBase :: [Text] -> Bool -> Int -> [Text] -> Maybe [Text]
absoluteBase _ _ 0 segs = Just segs
absoluteBase self isInit level segs =
    let package = bool (take (length self - 1) self) self isInit
        up = level - 1
     in guard (up <= length package) $> (take (length package - up) package <> segs)

idOf :: [Text] -> ModuleId
idOf = moduleIdUnsafe . ("python:" <>) . T.intercalate "."

-- | ADR 19: slash-segmented, @tiny/core@ for @tiny.core@.
nameOf :: [Text] -> ModuleName
nameOf = moduleNameUnsafe . T.intercalate "/"
