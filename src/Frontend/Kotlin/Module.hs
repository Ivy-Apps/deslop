{- | SPIKE: lowers Kotlin files to one 'Module' per package.

A package is the unit: it spans files (and Gradle modules), and code in one
file of a package uses the others with no import at all, so a file-level
Module would need a symbol table to find those edges. At package level they are
internal and simply vanish.

An import resolves to the longest prefix of its path that is a package some
parsed file declares: @a.b.C.Inner@ and the top-level function @a.b.fn@ both
land on @a.b@. Anything else is 'External' (the stdlib, a library, generated
code under @build/@).
-}
module Frontend.Kotlin.Module (lowerKt) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Deslop.Module
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask)
import FileSystem.Path (AbsPath (..), ProjectRoot, absPathUnsafe, relativePathTo)
import Frontend.Kotlin.Source (KtFile (..), KtImport (..), importsOf, packageOf)
import System.OsPath (takeDirectory)

type Package = [Text]

lowerKt :: (Reader ProjectRoot :> es) => [KtFile] -> Eff es [Module]
lowerKt files = do
    root <- ask @ProjectRoot
    pure . fmap (lowerPackage root known) . Map.toList $ byPackage
  where
    byPackage = Map.fromListWith (flip (<>)) [(packageOf f, [f]) | f <- sortOn (.path) files]
    known = Map.keysSet byPackage

lowerPackage :: ProjectRoot -> Set Package -> (Package, [KtFile]) -> Module
lowerPackage root known (pkg, files) =
    Module
        { id = packageId pkg
        , names = packageName pkg :| []
        , -- A package has no one directory: this is just where its first file is.
          path = relativePathTo root . absPathUnsafe . takeDirectory . (.osPath) . (.path) . NE.head $ fromList' files
        , edges = concatMap (edgesOf root known pkg) files
        }
  where
    fromList' = fromMaybe (error "a package always has a file") . nonEmpty

edgesOf :: ProjectRoot -> Set Package -> Package -> KtFile -> [DependencyEdge]
edgesOf root known self file = mapMaybe edge . importsOf $ file
  where
    edge (line, imp, code) = case resolve known imp of
        Just pkg | pkg == self -> Nothing
        Just pkg -> Just . mk imp line code $ Resolved (packageId pkg) (packageName pkg :| [])
        Nothing -> Just . mk imp line code . External . specifierOf $ imp
    mk imp line code target =
        DependencyEdge
            { specifier = specifierOf imp
            , target = target
            , kind = ImportEdge
            , location = Location {file = relativePathTo root file.path, line = line, code = code}
            }
    specifierOf imp = specifierUnsafe . T.intercalate "." $ imp.path <> ["*" | imp.wildcard]

resolve :: Set Package -> KtImport -> Maybe Package
resolve known imp
    | imp.wildcard = knownPkg imp.path
    | otherwise = listToMaybe . mapMaybe knownPkg . reverse . drop 1 . inits $ imp.path
  where
    knownPkg p = p <$ guard (p `Set.member` known)

packageId :: Package -> ModuleId
packageId = moduleIdUnsafe . ("kotlin-package:" <>) . T.intercalate "."

-- | Slash-segmented, per ADR 19. The default package needs some name.
packageName :: Package -> ModuleName
packageName [] = moduleNameUnsafe "<default>"
packageName pkg = moduleNameUnsafe . T.intercalate "/" $ pkg
