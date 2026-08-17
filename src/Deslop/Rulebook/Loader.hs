{-# LANGUAGE QuasiQuotes #-}

{- | Reading rulebooks off disk: the only part of the pipeline that touches IO.

Every @deslop\/rules\/*.yaml@ is read, decoded and compiled, and /all/ of their
failures are reported together. The report is grouped by file, then by rule,
then by field, because more than one rulebook can be broken in one run and an
error that does not say which file it came from is a treasure hunt.

'RulebookLoadError' deliberately does not carry the file it came from. Only the
loader knows that, and keeping it out means a single rulebook can be compiled
and its failure inspected without an absolute path getting into the answer.
It is named apart from @Types.RulebookError@, which is the error the whole run
fails with once this one has been rendered.
-}
module Deslop.Rulebook.Loader (
    loadRulebook,
    loadRulebookFrom,
    rulebookFromFile,
    RulebookLoadError (..),
    renderRulebookErrors,
) where

import Data.Text qualified as T
import Deslop.Rulebook (Rulebook)
import Deslop.Rulebook.Compiler (CompileError (..), compileRulebook, renderCompileError)
import Deslop.Rulebook.Dto (parseRulebookYaml)
import Effectful
import Effects.FileSystem (AbsPath (..), RoFileSystem, decodeOsPath, fsDirectoryExists, fsListDirectory, fsReadFile, withAbsBaseUnsafe)
import System.OsPath (OsPath, osp)
import UI (pluralise)

-- | Why one rulebook file could not become a 'Rulebook'.
data RulebookLoadError
    = -- | Not well-formed YAML, or not shaped like a rulebook at all.
      UnreadableYaml Text
    | -- | A rulebook, but some of its patterns do not compile.
      UncompilablePatterns (NonEmpty CompileError)
    deriving stock (Show, Eq)

rulesDir :: OsPath
rulesDir = [osp|deslop/rules|]

loadRulebook :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text [Rulebook])
loadRulebook projectPath = loadRulebookFrom (withAbsBaseUnsafe projectPath rulesDir)

{- | Loads every rulebook in a directory. A failure anywhere means none is
returned: enforcing half a rulebook would report problems its author never
asked for and miss the ones they did.
-}
loadRulebookFrom :: (RoFileSystem :> es) => AbsPath -> Eff es (Either Text [Rulebook])
loadRulebookFrom dir = fsDirectoryExists dir >>= bool (pure (Right [])) loadAll
  where
    loadAll = do
        paths <- fsListDirectory dir
        results <- traverse (\path -> (nameOf path,) <$> rulebookFromFile path) paths
        pure $ case nonEmpty [(name, err) | (name, Left err) <- results] of
            Just failures -> Left (renderRulebookErrors failures)
            Nothing -> Right [rulebook | (_, Right rulebook) <- results]

    nameOf path = decodeOsPath path.osPath

rulebookFromFile :: (RoFileSystem :> es) => AbsPath -> Eff es (Either RulebookLoadError Rulebook)
rulebookFromFile path = compile <$> fsReadFile path
  where
    compile bytes = do
        dto <- first UnreadableYaml (parseRulebookYaml bytes)
        first UncompilablePatterns (compileRulebook dto)

{- | Every failure of a run, grouped by file and then by rule, in source order
throughout - the author reads their file top to bottom and the report should
match. The count comes first, because "how bad is this" is the first question
anyone asks.
-}
renderRulebookErrors :: NonEmpty (Text, RulebookLoadError) -> Text
renderRulebookErrors failures =
    "Could not load "
        <> pluralise (length failures) "rulebook"
        <> ".\n\n"
        <> T.intercalate "\n\n" (uncurry renderOne <$> toList failures)

renderOne :: Text -> RulebookLoadError -> Text
renderOne name (UnreadableYaml detail) =
    name <> "\n" <> T.intercalate "\n" (("  " <>) <$> T.lines detail)
renderOne name (UncompilablePatterns errors) =
    name <> "\n" <> T.intercalate "\n" (renderRule <$> byRule (toList errors))
  where
    renderRule = T.intercalate "\n" . fmap renderCompileError

{- | Groups a rule's errors together while keeping every rule in the order it
appeared. Errors arrive in source order already, so consecutive runs of the
same rule are the groups - no sorting, which would scramble that order.
-}
byRule :: [CompileError] -> [[CompileError]]
byRule = foldr step []
  where
    step err (sameRule@(next : _) : rest)
        | next.rule == err.rule = (err : sameRule) : rest
    step err groups = [err] : groups
