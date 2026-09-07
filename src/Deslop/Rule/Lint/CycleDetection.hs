module Deslop.Rule.Lint.CycleDetection (
    noImportCycles,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.CodeGraph (GraphKey (..), ModuleCycle (..), ModuleGraph, findCycles, graphKeyOf)
import Deslop.Module (DependencyEdge (..), Location (..), Module (..), ModuleName (..), canonicalName)
import Deslop.Problem (LintRuleId (..), Problem (..))
import Effectful (Eff, type (:>))
import Effectful.Reader.Static (Reader)
import Effects.ReportProblem (ReportProblem, report)

{- | Reports the cycle against its start module, showing the loop it forms and
the import statement that enters it.
-}
importCycle :: ModuleCycle -> Problem
importCycle (ModuleCycle loop) =
    LintProblem
        { lintRule = LintRuleId "no-import-cycles"
        , location = enteringImport start nextHop
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

    {- | Where the loop is entered from. Taken from the edge itself, so the
    line is the statement's own rather than the module's first.

    The edge is found by the identity of what it resolved to, not by the text
    it was written with: the same module can be named several ways. A cycle is
    built only from parsed modules, so the edge is always there; the fallback
    names the target rather than inventing a line for it.
    -}
    enteringImport :: Module -> Module -> Location
    enteringImport importer target =
        maybe (whole importer target) (strip . (.location))
            . find ((== ModuleKey target.id) . graphKeyOf)
            $ importer.edges

    strip loc = loc {code = T.strip loc.code}
    whole importer target =
        Location {file = importer.path, line = 1, code = (canonicalName target).text}

noImportCycles ::
    ( Reader ModuleGraph :> es
    , ReportProblem :> es
    ) =>
    Eff es ()
noImportCycles = findCycles >>= traverse_ (report . importCycle)

-- | Renders the loop as a closed walk, repeating the start to show it closing.
renderLoop :: NonEmpty Module -> Text
renderLoop loop =
    T.intercalate " → "
        . map ((.text) . canonicalName)
        $ toList loop <> [NE.head loop]
