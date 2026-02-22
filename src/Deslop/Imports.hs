module Deslop.Imports (
    importAliases,
    resolveTsImport,
) where

import Control.Monad (when)
import Data.List (find, isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, asks)
import Effects.ReportProblem (Location (..), Problem (..), ReportProblem, RuleId (..), Severity (..), report)
import System.FilePath (
    joinPath,
    splitDirectories,
    takeDirectory,
    (</>),
 )
import TypeScript.AST (
    TsNode (Import, target),
    TsProgram (ast, path),
 )
import TypeScript.Config (
    ImportAlias (ImportAlias, label, path),
    TsConfig (paths),
 )
import Types (Renderable (render))
import Utils (safePop)

noRelativeImports :: (TsNode, TsNode) -> FilePath -> Problem
noRelativeImports (old, new) path =
    LintProblem
        { rule = RuleId "no-relative-imports"
        , location = Location {file = path, code = render old}
        , severity = Error
        , description = "Relative imports are not allowed. Use absolute path aliased ones."
        , fix = "Use ```" <> render new <> "``` instead."
        }

importAliases :: (Reader TsConfig :> es, ReportProblem :> es) => TsProgram -> Eff es TsProgram
importAliases prog = do
    ast' <- traverse fixImport prog.ast
    pure prog {ast = ast'}
  where
    fixImport old@(Import _ t _) = do
        t' <- fixTarget t
        let new = old {target = t'}
        when
            (t /= t')
            (report $ noRelativeImports (old, new) prog.path)
        pure new
    fixImport x = pure x

    fixTarget t = do
        as <- asks @TsConfig (.paths)
        let absT = T.pack . absPath as $ t
        case useAlias as absT of
            Just absT' -> pure . fst $ dropCommonPre (absT', absT)
            Nothing -> pure t

    dropCommonPre :: (Text, Text) -> (Text, Text)
    dropCommonPre (x, y) = case T.commonPrefixes x y of
        Just (_, x', y') -> (x', y')
        Nothing -> (x, y)

    useAlias as fp = applyAlias <$> findAliasForPath as
      where
        applyAlias (ImportAlias a p) = T.replace p a fp
        findAliasForPath = find (\a -> a.path `T.isInfixOf` fp)

    absPath as = resolveTsImport prog.path . T.unpack . reverseAlias as

    reverseAlias as fp =
        maybe fp (removeAlias fp)
            . find ((`T.isInfixOf` fp) . (.label))
            $ as
    removeAlias fp (ImportAlias l p) = T.replace l p fp

resolveTsImport :: FilePath -> FilePath -> FilePath
resolveTsImport sourcePath importPath
    | isBareSpecifier importPath = importPath
    | otherwise =
        let
            sourceDir = takeDirectory sourcePath
            rawCombined = sourceDir </> importPath
         in
            normalizeSegments rawCombined

isBareSpecifier :: String -> Bool
isBareSpecifier path = not (isRelative path || isAbsolute path)
  where
    isRelative p = "." `isPrefixOf` p -- Covers "./", "../", and just "."
    isAbsolute p = "/" `isPrefixOf` p

normalizeSegments :: FilePath -> FilePath
normalizeSegments = joinPath . reverse . foldl' step [] . splitDirectories
  where
    step stack segment
        | segment == "." = stack
        | segment == ".." = safePop stack
        | otherwise = segment : stack
