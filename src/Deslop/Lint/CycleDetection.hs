module Deslop.Lint.CycleDetection (
    noImportCycles,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleCycle (..), ModuleGraph, findCycles)
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, asks)
import Effects.FileSystem (AbsPath, relativePathTo)
import Effects.ReportProblem (ReportProblem, report)
import TypeScript.Config (TsConfig (..))
import TypeScript.ModuleResolver (ModuleId (..))

{- | Reports the cycle against its start module, showing the loop it forms and
the import statement that enters it.
-}
importCycle :: AbsPath -> ModuleCycle -> Problem
importCycle projectPath (ModuleCycle loop) =
    LintProblem
        { lintRule = LintRuleId "no-import-cycles"
        , location =
            Location
                { file = relativePathTo projectPath start.path
                , code = enteringImport start nextHop.id
                }
        , description = "Circular dependency (import cycle) detected: " <> renderLoop loop
        , fix =
            "Import cycles are not allowed. Break the loop by removing one of its"
                <> " imports - usually by extracting the shared code into a module that"
                <> " both sides can depend on."
        , autoFixable = False
        }
  where
    start = NE.head loop
    -- a module that imports itself is its own next hop
    nextHop = fromMaybe start . listToMaybe . NE.tail $ loop

    enteringImport :: AstModule -> ModuleId -> Text
    enteringImport importer target =
        maybe target.text (T.strip . (.rawStatement))
            . find ((== target) . (.target))
            $ importer.nodes

noImportCycles ::
    ( Reader ModuleGraph :> es
    , Reader TsConfig :> es
    , ReportProblem :> es
    ) =>
    Eff es ()
noImportCycles = do
    projectPath <- asks @TsConfig (.baseUrl)
    findCycles >>= traverse_ (report . importCycle projectPath)

-- | Renders the loop as a closed walk, repeating the start to show it closing.
renderLoop :: NonEmpty AstModule -> Text
renderLoop loop =
    T.intercalate " → "
        . map (.id.text)
        $ toList loop <> [NE.head loop]
