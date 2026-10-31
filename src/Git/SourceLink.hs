{- | Where a project's files can be read on the web, from what git knows about
its checkout.

Only GitHub and GitLab are recognised, by their exact host, @github.com@ and
@gitlab.com@; any other host has no link. Only GitHub's pull requests are linked
to, through what @gh@ says. What a project is called comes from the same remote,
on any host. Pure: asking git and gh is "Effects.Git"'s business.
-}
module Git.SourceLink (
    Branch (..),
    Checkout (..),
    Diff (..),
    DiffAnchors (..),
    Forge (..),
    GitRef (..),
    OriginUrl (..),
    ProjectName (..),
    PullRequest (..),
    Revision (..),
    SourceBase (..),
    SourceLinks (..),
    projectNameOf,
    pullRequestFromGh,
    sourceLinksOf,
) where

import Data.Aeson (decodeStrict, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text qualified as T
import FileSystem.Path (ProjectRelativePath, ProjectRoot (..), folderName)
import Numeric (showHex)

-- | What git accepts as a revision: a branch, a tag, a commit.
newtype GitRef = GitRef {text :: Text}
    deriving stock (Show, Eq, Ord)

{- | What a branch changed: the files that differ from where it left 'base',
the ref the diff was taken from.
-}
data Diff = Diff
    { base :: GitRef
    , files :: Set ProjectRelativePath
    }
    deriving stock (Show, Eq)

-- | A branch, by its short name: @feature/x@.
newtype Branch = Branch {name :: Text}
    deriving stock (Show, Eq)

-- | What is checked out.
data Revision
    = OnBranch Branch
    | -- | @HEAD@ is detached at this commit.
      Detached Text
    deriving stock (Show, Eq)

-- | The @origin@ remote, as configured: SSH, scp-like or https.
newtype OriginUrl = OriginUrl {text :: Text}
    deriving stock (Show, Eq)

-- | What git says about the checkout a project sits in.
data Checkout = Checkout
    { originUrl :: OriginUrl
    , revision :: Revision
    , prefix :: Text
    -- ^ The project's folder inside the repository, ending in @/@; empty at its root.
    }
    deriving stock (Show, Eq)

{- | The web URL a project-relative path is appended to, to read that file at
the checked-out revision. Ends in @/@.
-}
newtype SourceBase = SourceBase {url :: Text}
    deriving stock (Show, Eq)

{- | How a file's diff is found in a GitHub pull request: the anchor @#diff-@
followed by the SHA-256 of its path in the repository.
-}
newtype DiffAnchors = DiffAnchors
    { repoPrefix :: Text
    -- ^ The project's folder inside the repository, raw: a repository path is the prefix, then the project path.
    }
    deriving stock (Show, Eq)

-- | A pull request's web page. Its files' diffs are under @/files@.
newtype PullRequest = PullRequest {url :: Text}
    deriving stock (Show, Eq)

-- | The forge the project's @origin@ is on, with what only it can link to.
data Forge
    = -- | Its pull requests show a file's diff at an anchor.
      GitHub DiffAnchors
    | GitLab
    deriving stock (Show, Eq)

-- | Where a project's files, and their diff, can be read on the web.
data SourceLinks = SourceLinks
    { source :: SourceBase
    , revision :: Revision
    -- ^ What the links read the files at.
    , forge :: Forge
    }
    deriving stock (Show, Eq)

-- | What people call a project.
newtype ProjectName = ProjectName {text :: Text}
    deriving stock (Show, Eq)

{- | The repository's name in its @origin@ URL, on any host, so a subfolder or a
worktree of it is called the same. Without a name there, the project's folder.
-}
projectNameOf :: ProjectRoot -> Maybe OriginUrl -> ProjectName
projectNameOf root origin =
    ProjectName . fromMaybe (folderName root.path) $ do
        (_host, repoPath) <- hostAndPath =<< origin
        pure $ T.takeWhileEnd (/= '/') repoPath

-- | The links of a checkout, when its remote is on a forge we know.
sourceLinksOf :: Checkout -> Maybe SourceLinks
sourceLinksOf checkout = do
    (host, repoPath) <- hostAndPath checkout.originUrl
    forge <- forgeOf host
    let repoUrl = "https://" <> host <> "/" <> repoPath
    pure
        SourceLinks
            { source =
                SourceBase $
                    repoUrl
                        <> blobPath forge
                        <> encodePath (revisionText checkout.revision)
                        <> "/"
                        <> encodePath checkout.prefix
            , revision = checkout.revision
            , forge = forge
            }
  where
    forgeOf host = case T.toLower host of
        "github.com" -> Just . GitHub $ DiffAnchors {repoPrefix = checkout.prefix}
        "gitlab.com" -> Just GitLab
        _otherHost -> Nothing

revisionText :: Revision -> Text
revisionText = \case
    OnBranch branch -> branch.name
    Detached commit -> commit

-- | The pull request in what @gh pr view --json url@ prints. None when it names no url.
pullRequestFromGh :: ByteString -> Maybe PullRequest
pullRequestFromGh = parseMaybe (withObject "pull request" $ fmap PullRequest . (.: "url")) <=< decodeStrict

blobPath :: Forge -> Text
blobPath = \case
    GitHub _ -> "/blob/"
    GitLab -> "/-/blob/"

{- | The host and the @owner/repo@ path of a remote, from any of
@https://host/o/r.git@, @ssh://git\@host:22/o/r@ or @git\@host:o/r.git@, with
or without the newline git prints after it.
-}
hostAndPath :: OriginUrl -> Maybe (Text, Text)
hostAndPath origin = do
    (authority, rest) <- case T.breakOn "://" remote of
        (scheme, afterScheme)
            | not (T.null afterScheme) && scheme `elem` ["https", "http", "ssh", "git"] ->
                Just . second (T.drop 1) . T.breakOn "/" $ T.drop 3 afterScheme
            | not (T.null afterScheme) -> Nothing
        -- scp-like: [user@]host:path
        _ -> case T.breakOn ":" remote of
            (_, "") -> Nothing
            (authority, path) -> Just (authority, T.drop 1 path)
    let host = T.takeWhile (/= ':') . T.takeWhileEnd (/= '@') $ authority
        repoPath = T.dropWhileEnd (== '/') . stripGit . T.dropWhileEnd (== '/') . T.dropWhile (== '/') $ rest
    guard . not $ T.null host || T.null repoPath
    pure (host, repoPath)
  where
    remote = T.strip origin.text
    stripGit path = fromMaybe path $ T.stripSuffix ".git" path

-- | Percent-encodes each segment of a @/@-separated path, keeping the slashes.
encodePath :: Text -> Text
encodePath = T.intercalate "/" . fmap encodeSegment . T.splitOn "/"

encodeSegment :: Text -> Text
encodeSegment = foldMap escape . BS.unpack . encodeUtf8
  where
    escape byte
        | unreserved c = one c
        | otherwise = "%" <> T.toUpper (T.justifyRight 2 '0' . toText $ showHex byte "")
      where
        c = chr (fromIntegral byte)
    unreserved c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-._~" :: String)
