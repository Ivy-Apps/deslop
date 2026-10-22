{- | The built-in check that a module is named by an alias rather than by a
route from wherever the reader happens to be.

Imports and re-exports are the same problem and are fixed the same way, but they
are reported under two rule ids. A Lint Problem's id is @{rule}#{file}@, so one
shared id would mean that accepting a legacy relative import into the Baseline
also silenced - and stopped @deslop fix@ from repairing - every relative
re-export in that file.
-}
module Frontend.TypeScript.Lint.RelativeSpecifiers (
    noRelativeSpecifiers,
    relativeSpecifierRules,
) where

import Deslop.Frontend.Finding (Finding, fixable)
import Deslop.Module (Location (..), Specifier (..), specifierUnsafe)
import Deslop.Problem (LintRuleId (..), Problem (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.FileSystem (RoFileSystem)
import FileSystem.Path (ProjectRelativePath, ProjectRoot, relativePathTo)
import Frontend.TypeScript.Config (TsConfig)
import Frontend.TypeScript.CST (
    TsNode (..),
    TsProgram (cst, path),
    onLines,
    specifierOf,
    withSpecifier,
 )
import Frontend.TypeScript.ModuleResolver (isRelativeSpecifier, reverseResolveImport)
import Renderable (Renderable (render))

-- | Which built-in rule a node answers to, and what it says when broken.
data SpecifierRule = SpecifierRule
    { ruleId :: LintRuleId
    , description :: Text
    }

importRule :: SpecifierRule
importRule =
    SpecifierRule
        { ruleId = LintRuleId "no-relative-imports"
        , description = "Relative imports are not allowed. Use aliased ones."
        }

reExportRule :: SpecifierRule
reExportRule =
    SpecifierRule
        { ruleId = LintRuleId "no-relative-exports"
        , description = "Relative re-exports are not allowed. Use aliased ones."
        }

-- | Every rule 'noRelativeSpecifiers' reports under.
relativeSpecifierRules :: [LintRuleId]
relativeSpecifierRules = (.ruleId) <$> [importRule, reExportRule]

ruleFor :: TsNode -> Maybe SpecifierRule
ruleFor Import {} = Just importRule
ruleFor ReExport {} = Just reExportRule
ruleFor Source {} = Nothing

relativeSpecifier :: SpecifierRule -> (TsNode, TsNode) -> Int -> ProjectRelativePath -> Bool -> Problem
relativeSpecifier rule (old, new) line file autoFixable =
    LintProblem
        { lintRule = rule.ruleId
        , location = Location {file = file, line = line, code = render old}
        , description = rule.description
        , fix = "Use ```" <> render new <> "``` instead."
        , autoFixable
        }

{- | Every relative specifier in the program, each with the rewrite that names
the same module by its alias.
-}
noRelativeSpecifiers ::
    ( Reader TsConfig :> es
    , Reader ProjectRoot :> es
    , RoFileSystem :> es
    ) =>
    TsProgram -> Eff es [Finding TsProgram]
noRelativeSpecifiers prog = do
    projectRoot <- ask @ProjectRoot
    let file = relativePathTo projectRoot prog.path
    catMaybes <$> traverse (findingAt file) (zip [0 ..] (onLines prog.cst))
  where
    -- Only a specifier that is actually relative is this rule's business. One
    -- module answers to several names, so a written alias differing from the
    -- canonical one - '~/server/db' where '@/server/db' is canonical - is the
    -- same module named another way, not a relative import.
    findingAt file (index, (line, old)) = case (ruleFor old, specifierOf old) of
        (Just rule, Just t) | isRelativeSpecifier (specifierUnsafe t) -> do
            t' <- (.text) <$> reverseResolveImport prog.path (specifierUnsafe t)
            let new = withSpecifier t' old
            pure $
                fixable (relativeSpecifier rule (old, new) line file) (Endo $ replaceNode index new)
                    <$ guard (t /= t')
        _ -> pure Nothing

-- | The program with the node at that index, counted from zero, replaced.
replaceNode :: Int -> TsNode -> TsProgram -> TsProgram
replaceNode index new prog = prog {cst = zipWith pick [0 ..] prog.cst}
  where
    pick i node = bool node new (i == index)
