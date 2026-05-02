module Deslop.RelativeImports (
    importAliases,
    fixTarget,
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

noRelativeImports :: (TsNode, TsNode) -> AbsPath -> AbsPath -> Problem
noRelativeImports (old, new) projectPath modulePath =
    LintProblem
        { lintRule = LintRuleId "no-relative-imports"
        , location = Location {file = relativePathTo projectPath modulePath, code = render old}
        , description = "Relative imports are not allowed. Use aliased ones."
        , fix = "Use ```" <> render new <> "``` instead."
        }

importAliases ::
    ( Reader TsConfig :> es
    , Reader Baseline :> es
    , ReportProblem :> es
    , RoFileSystem :> es
    ) =>
    TsProgram -> Eff es TsProgram
importAliases prog = do
    cst' <- traverse fixImport prog.cst
    pure prog {cst = cst'}
  where
    fixImport old@(Import _ t _) = do
        t' <- (.text) <$> fixTarget prog.path t
        if (t /= t')
            then do
                let new = old {target = t'}
                projPath <- asks @TsConfig (.baseUrl)
                let problem = noRelativeImports (old, new) projPath prog.path
                report problem
                baseline <- ask @Baseline
                if inBaseline baseline problem
                    then pure old -- don't change baselined imports
                    else pure new
            else pure old
    fixImport x = pure x

fixTarget ::
    ( Reader TsConfig :> es
    , RoFileSystem :> es
    ) =>
    AbsPath -> Text -> Eff es ModuleId
fixTarget progPath t = do
    reverseResolveImport progPath (moduleIdUnsafe t)
