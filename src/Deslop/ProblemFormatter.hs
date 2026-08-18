module Deslop.ProblemFormatter (formatProblem) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.Problem (Location (..), Problem (..), ProblemId (..), ViolationKind (..), problemId)
import TypeScript.ModuleResolver (ModuleId (..))
import Utils (pluralise)

formatProblem :: Problem -> Text
formatProblem p@LintProblem {} =
    autoFix <> problemHeader <> description <> code <> fixText
  where
    autoFix = bool "" "[AUTO-FIXABLE] " p.autoFixable
    problemHeader = "# " <> (problemId p).text <> "\n"
    description = p.description <> "\n"
    code = "```ts\n" <> T.strip p.location.code <> "\n```\n"
    fixText = "FIX: " <> T.strip p.fix
formatProblem p@RuleViolation {} =
    problemHeader <> description <> fixText
  where
    problemHeader = "# " <> (problemId p).text <> "\n"
    description = p.prose <> "\n\n" <> violation p.badModule p.kind <> "\n"
    fixText = "FIX: " <> T.strip p.fix

{- | What the module did, in the Rule's own terms. Every sentence names the
module even though the header above it already does, so that a violation
quoted on its own still says who broke the Rule.
-}
violation :: ModuleId -> ViolationKind -> Text
violation badModule DirectImport {imported, importStatement} =
    "Module '"
        <> badModule.text
        <> "' directly imports '"
        <> imported.text
        <> "'."
        <> codeBlock importStatement
violation badModule TransitiveImport {chain, firstImport, alsoReached} =
    "Module '"
        <> badModule.text
        <> "' transitively imports '"
        <> (NE.last chain).text
        <> "' ("
        <> pluralise (NE.length chain - 1) "hop"
        <> ") via: "
        <> T.intercalate " → " (map (.text) (toList chain))
        <> "."
        <> maybe "" codeBlock firstImport
        <> absorbed (firstHop chain) alsoReached
violation badModule MissingUse {requiredImport, transitive} =
    "Module '"
        <> badModule.text
        <> "' must "
        <> bool "import '" "transitively import '" transitive
        <> requiredImport
        <> "'."
violation badModule MissingModule {requiredModule} =
    "Module '"
        <> badModule.text
        <> "' requires '"
        <> requiredModule.text
        <> "' to exist."

{- | What the compacted duplicates would have said. Their forbidden modules are
left out on purpose - they are what made the un-compacted report unreadable.
What the reader still has to act on is the set of imports at fault, so any hop
other than the one already shown above is named.
-}
absorbed :: Maybe ModuleId -> [NonEmpty ModuleId] -> Text
absorbed _ [] = ""
absorbed shownHop chains =
    "\nAlso reaches "
        <> pluralise (length chains) "more forbidden module"
        <> otherImports
        <> "."
  where
    otherImports = case ordNub . filter ((/= shownHop) . Just) . mapMaybe firstHop $ chains of
        [] -> " through this import"
        [hop] -> ", through the import of " <> quoted [hop]
        hops -> ", through the imports of " <> quoted hops
    quoted = T.intercalate ", " . map (\hop -> "'" <> hop.text <> "'")

-- | The import that opens a chain. Absent when the chain never leaves the module.
firstHop :: NonEmpty ModuleId -> Maybe ModuleId
firstHop = listToMaybe . drop 1 . toList

codeBlock :: Text -> Text
codeBlock statement = "\n```ts\n" <> statement <> "\n```"
