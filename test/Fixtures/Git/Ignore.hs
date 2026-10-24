-- | 'GitIgnore's loaded the way a run loads one, from @.gitignore@ files held in memory.
module Fixtures.Git.Ignore (
    gitIgnoreWith,
    rootGitIgnore,
) where

import Data.Text qualified as T
import Doubles.FileSystem (mockTree, runMockRoFileSystem)
import Effectful (runPureEff)
import FileSystem.Iterator (Entry)
import FileSystem.Path (AbsPath (..), absPathUnsafe, encodeOsPath)
import Git.Ignore (GitIgnore, loadGitIgnore)
import System.OsPath ((</>))

{- | The 'GitIgnore' of the project at the root, holding exactly these
@.gitignore@ files - each keyed by the project-relative directory it sits in,
@""@ for the root - in a language whose tooling generates what the predicate
names. Found by walking the project for them, as 'loadGitIgnore' always is.
-}
gitIgnoreWith :: (Entry -> Bool) -> AbsPath -> [(Text, Text)] -> GitIgnore
gitIgnoreWith ecosystem root scopes =
    runPureEff . runMockRoFileSystem (mockTree (fmap ignoreFile scopes)) $
        loadGitIgnore ecosystem root
  where
    ignoreFile (dir, content) = (inRoot (dirPrefix dir <> ".gitignore"), encodeUtf8 content)
    dirPrefix dir = bool (dir <> "/") "" (T.null dir)
    inRoot = absPathUnsafe . (root.osPath </>) . encodeOsPath

-- | As 'gitIgnoreWith', with one @.gitignore@ at the root.
rootGitIgnore :: (Entry -> Bool) -> AbsPath -> Text -> GitIgnore
rootGitIgnore ecosystem root content = gitIgnoreWith ecosystem root [("", content)]
