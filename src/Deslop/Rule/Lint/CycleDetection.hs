module Deslop.Rule.Lint.CycleDetection (
    noImportCycles,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..), ModuleName (..), canonicalName)
import Deslop.CodeGraph (GraphKey (..), ModuleCycle (..), ModuleGraph, findCycles, graphKeyOf)
import Deslop.Problem (LintRuleId (..), Location (..), Problem (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.ReportProblem (ReportProblem, report)
import FileSystem.Path (ProjectRoot, relativePathTo)

{- | Reports the cycle against its start module, showing the loop it forms and
the import statement that enters it.
-}
importCycle :: ProjectRoot -> ModuleCycle -> Problem
importCycle projectRoot (ModuleCycle loop) =
    LintProblem
        { lintRule = LintRuleId "no-import-cycles"
        , location =
            Location
                { file = relativePathTo projectRoot start.path
                , code = enteringImport start nextHop
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

    -- The edge is found by the identity of what it resolved to, not by the
    -- text it was written with: the same module can be named several ways.
    enteringImport :: AstModule -> AstModule -> Text
    enteringImport importer target =
        maybe (canonicalName target).text (T.strip . (.rawStatement))
            . find ((== InternalKey target.id) . graphKeyOf)
            $ importer.nodes

noImportCycles ::
    ( Reader ModuleGraph :> es
    , Reader ProjectRoot :> es
    , ReportProblem :> es
    ) =>
    Eff es ()
noImportCycles = do
    projectRoot <- ask @ProjectRoot
    findCycles >>= traverse_ (report . importCycle projectRoot)

-- | Renders the loop as a closed walk, repeating the start to show it closing.
renderLoop :: NonEmpty AstModule -> Text
renderLoop loop =
    T.intercalate " → "
        . map ((.text) . canonicalName)
        $ toList loop <> [NE.head loop]
