module Deslop.RelativeImports (
    importAliases,
    fixTarget,
) where

import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (RoFileSystem, fsMkAbsolute)
import Effects.ReportProblem (Location (..), Problem (..), ReportProblem, RuleId (..), Severity (..), report)
import System.OsPath (OsPath)
import TypeScript.CST (
    TsNode (Import, target),
    TsProgram (cst, path),
 )
import TypeScript.Config (
    TsConfig,
 )
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe, reverseResolveImport)
import Types (Renderable (render))

noRelativeImports :: (TsNode, TsNode) -> OsPath -> Problem
noRelativeImports (old, new) path =
    LintProblem
        { rule = RuleId "no-relative-imports"
        , location = Location {file = path, code = render old}
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
    OsPath -> Text -> Eff es ModuleId
fixTarget progPath t = do
    absTsModulePath <- fsMkAbsolute progPath
    reverseResolveImport absTsModulePath (moduleIdUnsafe t)
