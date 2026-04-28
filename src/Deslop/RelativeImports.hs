module Deslop.RelativeImports (
    importAliases,
    fixTarget,
) where

import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (AbsPath (..), RoFileSystem)
import Effects.ReportProblem (LintRuleId (..), Location (..), Problem (..), ReportProblem, Severity (..), report)
import TypeScript.CST (
    TsNode (Import, target),
    TsProgram (cst, path),
 )
import TypeScript.Config (
    TsConfig,
 )
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe, reverseResolveImport)
import Types (Renderable (render))

noRelativeImports :: (TsNode, TsNode) -> AbsPath -> Problem
noRelativeImports (old, new) path =
    LintProblem
        { lintRule = LintRuleId "no-relative-imports"
        , location = Location {file = path.osPath, code = render old}
        , severity = Error
        , description = "Relative imports are not allowed. Use absolute path aliased ones."
        , fix = "Use ```" <> render new <> "``` instead."
        }

importAliases ::
    ( Reader TsConfig :> es
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
        let new = old {target = t'}
        when
            (t /= t')
            (report $ noRelativeImports (old, new) prog.path)
        pure new
    fixImport x = pure x

fixTarget ::
    ( Reader TsConfig :> es
    , RoFileSystem :> es
    ) =>
    AbsPath -> Text -> Eff es ModuleId
fixTarget progPath t = do
    reverseResolveImport progPath (moduleIdUnsafe t)
