{- | The built-in check that a module is named by an alias rather than by a
route from wherever the reader happens to be.

Imports and re-exports are the same problem and are fixed the same way, but they
are reported under two rule ids. A Lint Problem's id is @{rule}#{file}@, so one
shared id would mean that accepting a legacy relative import into the Baseline
also silenced - and stopped @deslop fix@ from repairing - every relative
re-export in that file.
-}
module TypeScript.Lint.RelativeSpecifiers (
    noRelativeSpecifiers,
) where

import Deslop.Module (Location (..), Specifier (..), specifierUnsafe)
import Deslop.Problem (LintRuleId (..), Problem (..))
import Deslop.Problem.Baseline (Baseline, inBaseline)
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (RoFileSystem)
import Effects.ReportProblem (ReportProblem, report)
import FileSystem.Path (ProjectRelativePath, ProjectRoot, relativePathTo)
import Renderable (Renderable (render))
import TypeScript.Config (TsConfig)
import TypeScript.CST (
    TsNode (..),
    TsProgram (cst, path),
    onLines,
    specifierOf,
    withSpecifier,
 )
import TypeScript.ModuleResolver (isRelativeSpecifier, reverseResolveImport)

-- | Which built-in rule a node answers to, and what it says when broken.
data SpecifierRule = SpecifierRule
    { ruleId :: LintRuleId
    , description :: Text
    }

ruleFor :: TsNode -> Maybe SpecifierRule
ruleFor Import {} =
    Just
        SpecifierRule
            { ruleId = LintRuleId "no-relative-imports"
            , description = "Relative imports are not allowed. Use aliased ones."
            }
ruleFor ReExport {} =
    Just
        SpecifierRule
            { ruleId = LintRuleId "no-relative-exports"
            , description = "Relative re-exports are not allowed. Use aliased ones."
            }
ruleFor Source {} = Nothing

relativeSpecifier :: SpecifierRule -> (TsNode, TsNode) -> Int -> ProjectRelativePath -> Problem
relativeSpecifier rule (old, new) line file =
    LintProblem
        { lintRule = rule.ruleId
        , location = Location {file = file, line = line, code = render old}
        , description = rule.description
        , fix = "Use ```" <> render new <> "``` instead."
        , autoFixable = True
        }

noRelativeSpecifiers ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , Reader Baseline :> es
    , ReportProblem :> es
    , RoFileSystem :> es
    ) =>
    TsProgram -> Eff es TsProgram
noRelativeSpecifiers prog = do
    projectRoot <- ask @ProjectRoot
    let file = relativePathTo projectRoot prog.path
    cst' <- traverse (fixSpecifier file) (onLines prog.cst)
    pure prog {cst = cst'}
  where
    -- Only a specifier that is actually relative is this rule's business. One
    -- module answers to several names, so a written alias differing from the
    -- canonical one - '~/server/db' where '@/server/db' is canonical - is the
    -- same module named another way, not a relative import.
    fixSpecifier file (line, old) = case (ruleFor old, specifierOf old) of
        (Just rule, Just t) | isRelativeSpecifier (specifierUnsafe t) -> do
            t' <- (.text) <$> reverseResolveImport prog.path (specifierUnsafe t)
            if t /= t'
                then do
                    let new = withSpecifier t' old
                    let problem = relativeSpecifier rule (old, new) line file
                    report problem
                    baseline <- ask @Baseline
                    if inBaseline baseline problem
                        then pure old -- don't change baselined specifiers
                        else pure new
                else pure old
        _ -> pure old
