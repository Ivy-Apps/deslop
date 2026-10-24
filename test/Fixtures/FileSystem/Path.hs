{- | The project every in-memory spec runs against, at @\/project@, so that a
path in one spec's fake filesystem means the same as in the next one's.
-}
module Fixtures.FileSystem.Path (
    projectRoot,
    inProject,
) where

import FileSystem.Path (AbsPath, ProjectRoot (..))
import TestUtils (ap)

projectRoot :: ProjectRoot
projectRoot = ProjectRoot (ap "/project")

-- | A path in the project, e.g. @inProject "src/a.ts"@.
inProject :: Text -> AbsPath
inProject = ap . ("/project/" <>)
