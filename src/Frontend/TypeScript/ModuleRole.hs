{- | Whether a TypeScript file ships, tests or measures, judged by its path the
way the ecosystem's tools find them: Jest and Vitest by @*.test.*@ and
@__tests__@, Storybook by @*.stories.*@, Cypress and Playwright by their own
folders, Vitest's bench mode by @*.bench.*@.
-}
module Frontend.TypeScript.ModuleRole (moduleRole) where

import Data.Text qualified as T
import Deslop.Module (ModuleRole (..))
import FileSystem.Path (ProjectRelativePath, portablePath)

{- | A file is a 'Benchmark' when its name or a folder above it says so, else a
'Test' when its name or a folder above it says so, else 'Source'.
-}
moduleRole :: ProjectRelativePath -> ModuleRole
moduleRole path = foldMap roleOf [Benchmark, Test]
  where
    segments = T.splitOn "/" (portablePath path)
    folders = fromMaybe [] (viaNonEmpty init segments)
    -- Every dot-separated part of the file name but the first and the last:
    -- @b.test.ts@ has @test@, @b.ts@ has none.
    infixes = maybe [] (drop 1 . fromMaybe [] . viaNonEmpty init . T.splitOn ".") (viaNonEmpty last segments)
    roleOf role
        | any (`elem` infixesOf role) infixes || any (`elem` foldersOf role) folders = role
        | otherwise = Source

infixesOf :: ModuleRole -> [Text]
infixesOf = \case
    Source -> []
    Test -> ["spec", "test", "stories", "e2e", "cy"]
    Benchmark -> ["bench"]

foldersOf :: ModuleRole -> [Text]
foldersOf = \case
    Source -> []
    Test -> ["__tests__", "__mocks__", "test", "tests", "e2e", "cypress", "playwright"]
    Benchmark -> ["bench", "benchmark", "benchmarks"]
