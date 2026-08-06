module Deslop.Lint.RelativeImports (
    noRelativeImports,
) where

import Deslop.Baseline (Baseline, inBaseline)
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask, asks)
import Effects.FileSystem (AbsPath (..), RoFileSystem, relativePathTo)
import Effects.ReportProblem (ReportProblem, report)
import TypeScript.CST (
    TsNode (Import, target),
    TsProgram (cst, path),
 )
import TypeScript.Config (TsConfig (..))
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe, reverseResolveImport)
import Types (Renderable (render))

relativeImport :: (TsNode, TsNode) -> AbsPath -> AbsPath -> Problem
relativeImport (old, new) projectPath modulePath =
    LintProblem
        { lintRule = LintRuleId "no-relative-imports"
        , location = Location {file = relativePathTo projectPath modulePath, code = render old}
        , description = "Relative imports are not allowed. Use aliased ones."
        , fix = "Use ```" <> render new <> "``` instead."
        , autoFixable = True
        }

noRelativeImports ::
    ( Reader TsConfig :> es
    , Reader Baseline :> es
    , ReportProblem :> es
    , RoFileSystem :> es
    ) =>
    TsProgram -> Eff es TsProgram
noRelativeImports prog = do
    cst' <- traverse fixImport prog.cst
    pure prog {cst = cst'}
  where
    fixImport old@(Import _ t _) = do
        t' <- (.text) <$> reverseResolveImport prog.path (moduleIdUnsafe t)
        if t /= t'
            then do
                let new = old {target = t'}
                projPath <- asks @TsConfig (.baseUrl)
                let problem = relativeImport (old, new) projPath prog.path
                report problem
                baseline <- ask @Baseline
                if inBaseline baseline problem
                    then pure old -- don't change baselined imports
                    else pure new
            else pure old
    fixImport x = pure x
