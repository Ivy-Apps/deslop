{- | Whether a TypeScript file ships, tests or measures, judged by its path the
way the ecosystem's tools find them: Jest and Vitest by @*.test.*@ and
@__tests__@, Storybook by @*.stories.*@, Cypress and Playwright by their own
folders, Vitest's bench mode by @*.bench.*@.

A test named as one, @*.test.*@ or @*.spec.*@, also names what it tests: the
file of the same name beside it, or the same path under a test folder.
-}
module Frontend.TypeScript.ModuleRole (
    moduleRole,
    withSubjects,
) where

import Data.List (isSuffixOf)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Deslop.Module (Module (..), ModuleId, ModuleRole (..))
import FileSystem.Path (ProjectRelativePath, portablePath, portableSegments)

{- | A file is a 'Benchmark' when its name or a folder above it says so, else a
'Test' when its name or a folder above it says so, else 'Source'. Which file a
test tests takes the whole program: see 'withSubjects'.
-}
moduleRole :: ProjectRelativePath -> ModuleRole
moduleRole path = foldMap claim [(Benchmark, ["bench"], ["bench", "benchmark", "benchmarks"]), (Test Nothing, testInfixes, testFolders)]
  where
    segments = T.splitOn "/" (portablePath path)
    folders = fromMaybe [] (viaNonEmpty init segments)
    -- Every dot-separated part of the file name but the first and the last:
    -- @b.test.ts@ has @test@, @b.ts@ has none.
    infixes = maybe [] (drop 1 . fromMaybe [] . viaNonEmpty init . T.splitOn ".") (viaNonEmpty last segments)
    claim (role, names, dirs)
        | any (`elem` names) infixes || any (`elem` dirs) folders = role
        | otherwise = Source

testInfixes :: [Text]
testInfixes = ["spec", "test", "stories", "e2e", "cy"]

testFolders :: [Text]
testFolders = ["__tests__", "__mocks__", "test", "tests", "e2e", "cypress", "playwright"]

{- | Every test named after a source file, given that file as its subject: a
test tests the file whose path, without its extension, ends in the test's own
path once its test folder and its @.test@ or @.spec@ are dropped. So
@src\/a\/b.test.ts@ tests @src\/a\/b.ts@, and @test\/a\/b.spec.ts@ tests
@src\/a\/b.ts@. When several files match, the one nearest the root does.
-}
withSubjects :: [Module] -> [Module]
withSubjects modules = named <$> modules
  where
    sources = sortOn fst [((length s, portablePath m.path), (s, m.id)) | m <- modules, Source <- [m.role], Just s <- [stemmed <$> portableSegments m.path]]
    named m = case m.role of
        Test _ -> m {role = Test (subjectOf =<< subjectKey =<< portableSegments m.path)}
        _ -> m
    subjectOf :: [Text] -> Maybe ModuleId
    subjectOf key = viaNonEmpty head [mid | (_, (s, mid)) <- sources, key `isSuffixOf` s]

    stemmed segs = NE.init segs <> [stem $ NE.last segs]

{- | The path a test's subject ends in, when it is named as a test of one. A
helper in a test folder that is named after nothing tests nothing.
-}
subjectKey :: NonEmpty Text -> Maybe [Text]
subjectKey segs = (\file -> folders <> [file]) <$> testSuffix (stem $ NE.last segs)
  where
    folders = filter (/= "__tests__") . dropTestRoot $ NE.init segs
    dropTestRoot = \case
        (top : rest) | top `elem` ["test", "tests", "spec", "specs"] -> rest
        other -> other

-- | A file's name without its extension.
stem :: Text -> Text
stem file = case T.breakOnEnd "." file of
    (before, _) | T.length before > 1 -> T.dropEnd 1 before
    _ -> file

-- | The subject's name, when a file's name, without its extension, is that of a test of it.
testSuffix :: Text -> Maybe Text
testSuffix name = viaNonEmpty head $ mapMaybe (\s -> T.stripSuffix s name >>= nonBlank) [".test", ".spec"]
  where
    nonBlank t = if T.null t then Nothing else Just t
