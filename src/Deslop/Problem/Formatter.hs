module Deslop.Problem.Formatter (
    CodeFence (..),
    formatProblem,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.Module (EdgeKind (..), Location (..), ModuleName (..), Specifier (..))
import Deslop.Problem (Problem (..), ProblemId (..), ViolationKind (..), problemId)
import FileSystem.Path (ProjectRelativePath, portablePath)
import Utils (pluralise)

{- | The Markdown info string a quoted statement is fenced with - @ts@ for
TypeScript - so that whoever reads a report sees it highlighted in its language.
-}
newtype CodeFence = CodeFence Text
    deriving stock (Show, Eq)

formatProblem :: CodeFence -> Problem -> Text
formatProblem fence p@LintProblem {} =
    autoFix <> problemHeader <> whereLine <> description <> code <> fixText
  where
    autoFix = bool "" "[AUTO-FIXABLE] " p.autoFixable
    problemHeader = "# " <> (problemId p).text <> "\n"
    whereLine = at p.location <> "\n"
    description = p.description <> "\n"
    code = fenced fence (T.strip p.location.code) <> "\n"
    fixText = "FIX: " <> T.strip p.fix
formatProblem fence p@RuleViolation {} =
    problemHeader <> whereLine <> description <> fixText
  where
    problemHeader = "# " <> (problemId p).text <> "\n"
    whereLine = whereViolated p.modulePath p.kind <> "\n"
    description = p.prose <> "\n\n" <> violation fence p.badModule p.kind <> "\n"
    fixText = "FIX: " <> T.strip p.fix

{- | Where to send the reader. A violation with a statement points at that
statement; one whose complaint is that no statement exists can only point at
the module, and says so by naming a file and no line.
-}
whereViolated :: ProjectRelativePath -> ViolationKind -> Text
whereViolated _ DirectImport {location} = at location
whereViolated modulePath TransitiveImport {firstImport} =
    maybe (portablePath modulePath) at firstImport
whereViolated _ UnjudgedImport {location} = at location
whereViolated modulePath MissingUse {} = portablePath modulePath
whereViolated modulePath MissingModule {} = portablePath modulePath

-- | @src\/app\/page.ts:3@, the spelling an editor and a terminal both follow.
at :: Location -> Text
at loc = portablePath loc.file <> ":" <> show loc.line

{- | What the module did, in the Rule's own terms. Every sentence names the
module even though the header above it already does, so that a violation
quoted on its own still says who broke the Rule.
-}
violation :: CodeFence -> ModuleName -> ViolationKind -> Text
violation fence badModule DirectImport {imported, edge, location} =
    "Module '"
        <> badModule.text
        <> "' "
        <> verb edge
        <> " '"
        <> imported.text
        <> "'."
        <> codeBlock fence location.code
  where
    verb ImportEdge = "directly imports"
    verb ReExportEdge = "re-exports"
violation fence badModule TransitiveImport {chain, firstImport, alsoReached} =
    "Module '"
        <> badModule.text
        <> "' transitively imports '"
        <> (NE.last chain).text
        <> "'"
        <> via chain
        <> "."
        <> maybe "" (codeBlock fence . (.code)) firstImport
        <> absorbed (firstHop chain) alsoReached
violation fence badModule UnjudgedImport {chain, specifier, edge, location} =
    "Module '"
        <> badModule.text
        <> "' "
        <> verb edge
        <> " '"
        <> specifier.text
        <> "', which Deslop cannot resolve to a module, so whether it breaks this Rule cannot be judged."
        <> codeBlock fence location.code
        <> reachedFrom
  where
    -- How a target of the Rule gets to the module that wrote the dependency,
    -- unless it wrote it itself.
    reachedFrom = case NE.tail chain of
        [] -> ""
        _ -> "\nReached from '" <> (NE.head chain).text <> "'" <> via chain <> "."
    verb ImportEdge = "imports"
    verb ReExportEdge = "re-exports"
violation _ badModule MissingUse {requiredImport, transitive} =
    "Module '"
        <> badModule.text
        <> "' must "
        <> bool "import '" "transitively import '" transitive
        <> requiredImport
        <> "'."
violation _ badModule MissingModule {requiredModule} =
    "Module '"
        <> badModule.text
        <> "' requires '"
        <> requiredModule.text
        <> "' to exist."

-- | How a chain gets where it goes: @ (2 hops) via: a → b → c@.
via :: NonEmpty ModuleName -> Text
via chain =
    " ("
        <> pluralise (NE.length chain - 1) "hop"
        <> ") via: "
        <> T.intercalate " → " (map (.text) (toList chain))

{- | What the compacted duplicates would have said. Their forbidden modules are
left out on purpose - they are what made the un-compacted report unreadable.
What the reader still has to act on is the set of imports at fault, so any hop
other than the one already shown above is named.
-}
absorbed :: Maybe ModuleName -> [NonEmpty ModuleName] -> Text
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
firstHop :: NonEmpty ModuleName -> Maybe ModuleName
firstHop = listToMaybe . drop 1 . toList

codeBlock :: CodeFence -> Text -> Text
codeBlock fence = ("\n" <>) . fenced fence

fenced :: CodeFence -> Text -> Text
fenced (CodeFence info) statement = "```" <> info <> "\n" <> statement <> "\n```"
