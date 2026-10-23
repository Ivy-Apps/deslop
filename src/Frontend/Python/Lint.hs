{- | SPIKE: the one built-in Python lint, @no-relative-imports@, which rewrites
@from .helpers import x@ to @from tiny.helpers import x@.

It needs the importing module's dotted name, which 'PyConfig' and the unit's
path are enough to give: no other unit is consulted.
-}
module Frontend.Python.Lint (
    noRelativeImports,
    pythonRules,
) where

import Data.Text qualified as T
import Deslop.Frontend.Finding (Finding, fixable)
import Deslop.Module (Location (..))
import Deslop.Problem (LintRuleId (..), Problem (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask)
import FileSystem.Path (ProjectRelativePath, ProjectRoot, relativePathTo)
import Frontend.Python.Config (PyConfig, isPackageInit, qualifiedName)
import Frontend.Python.Module (absoluteBase)
import Frontend.Python.Parser (PyImport (..), PyLine (..), PyUnit (..))

ruleId :: LintRuleId
ruleId = LintRuleId "no-relative-imports"

pythonRules :: [LintRuleId]
pythonRules = [ruleId]

noRelativeImports :: (Reader ProjectRoot :> es) => PyConfig -> PyUnit -> Eff es [Finding PyUnit]
noRelativeImports cfg unit = do
    root <- ask @ProjectRoot
    let file = relativePathTo root unit.path
    pure . catMaybes . zipWith (findingAt file) [0 ..] $ unit.lines
  where
    self = qualifiedName cfg unit.path
    isInit = isPackageInit unit.path

    findingAt file index line = case line.stmt of
        Just imp@FromImport {level}
            | level > 0
            , Just base <- absoluteBase self isInit level imp.modSegs
            , not (null base) ->
                let new = "from " <> T.intercalate "." base <> " import " <> imp.afterImport
                    newLine = PyLine (encodeUtf8 new) (Just imp {level = 0, modSegs = base})
                 in Just $ fixable (problem file (index + 1) line new) (Endo $ replaceLine index newLine)
        _ -> Nothing

    problem :: ProjectRelativePath -> Int -> PyLine -> Text -> Bool -> Problem
    problem file lineNo old new autoFixable =
        LintProblem
            { lintRule = ruleId
            , location = Location {file = file, line = lineNo, code = decodeUtf8 old.raw}
            , description = "Relative imports are not allowed. Use absolute ones."
            , fix = "Use ```" <> new <> "``` instead."
            , autoFixable
            }

replaceLine :: Int -> PyLine -> PyUnit -> PyUnit
replaceLine index new unit = unit {lines = zipWith pick [0 ..] unit.lines}
  where
    pick i old = bool old new (i == index)
