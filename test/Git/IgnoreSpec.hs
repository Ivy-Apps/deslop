module Git.IgnoreSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Effects.FileSystem (AbsPath)
import FileSystem.Iterator (Entry (..))
import Git.Ignore (
    CharMatch (..),
    ClassItem (..),
    GitIgnore (..),
    IgnorePattern (..),
    IgnoreRule (..),
    IgnoreScope (..),
    Seg (..),
    Tok (..),
    alwaysIgnored,
    isIgnored,
    parseIgnoreFile,
    parseIgnoreRule,
    renderIgnoreRule,
 )
import Hedgehog (Gen, discard, forAll, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Hspec
import TestUtils (ap, prop)
import UnliftIO.Temporary (withSystemTempDirectory)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

projectRoot :: AbsPath
projectRoot = ap "/proj"

inProject :: Text -> AbsPath
inProject "" = projectRoot
inProject p = ap ("/proj/" <> p)

{- | Builds a 'GitIgnore' from @.gitignore@ contents keyed by project-relative
directory. Scopes must be listed shallowest-first, as 'loadGitIgnore' sorts them.
-}
gitIgnoreOf :: [(Text, Text)] -> GitIgnore
gitIgnoreOf ss = GitIgnore {root = projectRoot, scopes = fmap mkScope ss}
  where
    mkScope (dir, content) = IgnoreScope (inProject dir) (parseIgnoreFile content)

rootOnly :: Text -> GitIgnore
rootOnly content = gitIgnoreOf [("", content)]

ignoresFile :: GitIgnore -> Text -> Bool
ignoresFile gi p = isIgnored gi Entry {path = inProject p, isDir = False}

ignoresDir :: GitIgnore -> Text -> Bool
ignoresDir gi p = isIgnored gi Entry {path = inProject p, isDir = True}

-- | Interprets a trailing slash as "ask about a directory", exactly as git does.
ignoresQuery :: GitIgnore -> Text -> Bool
ignoresQuery gi q = maybe (ignoresFile gi q) (ignoresDir gi) . T.stripSuffix "/" $ q

--------------------------------------------------------------------------------
-- git as an oracle
--------------------------------------------------------------------------------

{- | A generated file tree, split into the directories it implies and its files.

Keeping the two apart is what makes the oracle trustworthy: @git check-ignore@
reads directory-ness off the filesystem rather than from a trailing slash, so
the tree has to actually exist on disk for directory-only rules to be exercised.
-}
data Tree = Tree
    { dirs :: [Text]
    , files :: [Text]
    }
    deriving (Show)

-- | Any path that is a proper ancestor of another is a directory, not a file.
mkTree :: [Text] -> Tree
mkTree paths =
    Tree
        { dirs = sort . toList $ dirSet
        , files = sort . filter (`Set.notMember` dirSet) . ordNub $ paths
        }
  where
    dirSet = Set.fromList . concatMap properAncestors $ paths
    properAncestors p =
        let segs = T.splitOn "/" p
         in fmap (\n -> T.intercalate "/" (take n segs)) [1 .. length segs - 1]

treePaths :: Tree -> [Text]
treePaths t = t.dirs <> t.files

{- | The subset of a tree's paths that real git considers ignored.

Global and system git config are neutralised, so a developer's personal
@core.excludesFile@ cannot pollute the oracle.
-}
gitIgnoredSet :: [(Text, Text)] -> Tree -> IO (Set Text)
gitIgnoredSet scopes tree = withSystemTempDirectory "deslop-gitignore" $ \dir -> do
    env' <- isolatedGitEnv
    _ <- git env' dir ["init", "-q"] ""
    traverse_ (writeScope dir) scopes
    traverse_ (createDirectoryIfMissing True . at dir) tree.dirs
    traverse_ (touch dir) tree.files
    checkIgnore env' dir
  where
    at dir p = dir <> "/" <> toString p

    writeScope dir (sub, content) = do
        createDirectoryIfMissing True (at dir sub)
        writeFileText (at dir sub <> "/.gitignore") content

    touch dir p = do
        createDirectoryIfMissing True (takeDirectory (at dir p))
        writeFileText (at dir p) ""

    checkIgnore env' dir = do
        let stdin' = toString . T.unlines . treePaths $ tree
        (code, out, err) <- git env' dir ["check-ignore", "--stdin"] stdin'
        -- Exit 1 simply means "nothing matched", which is not an error.
        case code of
            ExitFailure n | n > 1 -> fail ("git check-ignore failed: " <> err)
            _ -> pure . Set.fromList . filter (not . T.null) . lines . toText $ out

    git env' dir args =
        readCreateProcessWithExitCode
            (proc "git" (["-C", dir] <> args)) {env = Just env'}

isolatedGitEnv :: IO [(String, String)]
isolatedGitEnv = do
    inherited <- getEnvironment
    pure $ overrides <> filter ((`notElem` fmap fst overrides) . fst) inherited
  where
    overrides =
        [ ("GIT_CONFIG_GLOBAL", "/dev/null")
        , ("GIT_CONFIG_SYSTEM", "/dev/null")
        , ("GIT_CONFIG_NOSYSTEM", "1")
        ]

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

{- | Names deliberately disjoint from 'Git.Ignore.alwaysIgnoredNames': those are
ignored unconditionally by us and not by git, so they would break the oracle.
-}
genName :: Gen Text
genName = Gen.element ["src", "app", "lib", "a", "b", "foo.ts", "bar.log", "x.gen.ts", "test"]

genSegPattern :: Gen Text
genSegPattern =
    Gen.choice
        [ genName
        , pure "*"
        , pure "**"
        , pure "?"
        , pure "*.ts"
        , pure "[ab]"
        , pure "[!ab]"
        , pure "[a-z]"
        , (<> "*") <$> genName
        , ("*" <>) <$> genName
        ]

-- | A rule line that never begins with @!@, so callers can safely negate it.
genPositiveLine :: Gen Text
genPositiveLine = do
    anchored <- Gen.bool
    dirOnly <- Gen.bool
    segs <- Gen.list (Range.linear 1 3) genSegPattern
    pure $ bool "" "/" anchored <> T.intercalate "/" segs <> bool "" "/" dirOnly

-- | A rule with no @/@ at all, so it matches a basename at any depth.
genUnanchoredLine :: Gen Text
genUnanchoredLine = genSegPattern

genRelPath :: Gen Text
genRelPath = T.intercalate "/" <$> Gen.list (Range.linear 1 4) genName

genQuery :: Gen Text
genQuery = do
    p <- genRelPath
    isDir <- Gen.bool
    pure $ p <> bool "" "/" isDir

--------------------------------------------------------------------------------
-- Canonical rule generator, for the parse . render round trip
--------------------------------------------------------------------------------

{- | Generates only rules the parser could itself have produced.

The parser normalises: a metacharacter-free segment becomes 'Exact', and an
unanchored rule always holds exactly one segment.
-}
genIgnoreRule :: Gen IgnoreRule
genIgnoreRule = do
    negated <- Gen.bool
    dirOnly <- Gen.bool
    anchored <- Gen.bool
    segs <-
        if anchored
            then Gen.list (Range.linear 1 3) genSeg
            else one <$> genSeg
    pure IgnoreRule {pattern = IgnorePattern segs, negated, dirOnly, anchored}

genSeg :: Gen Seg
genSeg = Gen.choice [pure GlobStar, Exact <$> genLiteralText, Seg <$> genMetaToks]
  where
    genLiteralText = Gen.text (Range.linear 1 6) (Gen.element literalChars)

    -- At least one non-literal token, otherwise the parser would emit 'Exact'.
    genMetaToks = Gen.filter (/= [Star, Star]) $ do
        lead <- Gen.list (Range.linear 0 2) genTok
        meta <- genMetaTok
        trail <- Gen.list (Range.linear 0 2) genTok
        pure $ lead <> [meta] <> trail

    genTok = Gen.choice [genMetaTok, One . Lit <$> Gen.element literalChars]
    genMetaTok = Gen.choice [pure Star, pure (One AnyChar), One <$> genClass]

    genClass = Class <$> Gen.bool <*> Gen.list (Range.linear 1 3) genClassItem
    genClassItem =
        Gen.choice
            [ ClassChar <$> Gen.element literalChars
            , ClassRange <$> Gen.element ['a', 'b', 'x'] <*> Gen.element ['y', 'z']
            , ClassPosix <$> Gen.element [minBound .. maxBound]
            ]

    literalChars = "abz.-_# !" :: [Char]

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Git.Ignore" $ do
    parsingSpec
    matchingSpec
    scopeSpec
    alwaysIgnoredSpec
    orderingProps
    anchoringProps
    lexerProps
    differentialProps

parsingSpec :: Spec
parsingSpec = describe "parsing" $ do
    it "skips blank lines and comments" $
        parseIgnoreFile "# a comment\n\n   \nfoo\n" `shouldBe` [litRule "foo"]

    it "treats an escaped hash as a literal" $
        (fmap (.pattern) . parseIgnoreRule) "\\#foo" `shouldBe` Just (IgnorePattern [Exact "#foo"])

    it "treats an escaped bang as a literal" $
        parseIgnoreRule "\\!foo" `shouldBe` Just (litRule "!foo")

    it "reads a leading bang as negation" $
        (fmap (.negated) . parseIgnoreRule) "!foo" `shouldBe` Just True

    it "reads a trailing slash as directory-only" $
        (fmap (.dirOnly) . parseIgnoreRule) "build/" `shouldBe` Just True

    it "does not anchor a pattern whose only slash was the directory marker" $
        (fmap (.anchored) . parseIgnoreRule) "build/" `shouldBe` Just False

    it "anchors on a leading slash" $
        (fmap (.anchored) . parseIgnoreRule) "/build" `shouldBe` Just True

    it "anchors on an interior slash" $
        (fmap (.anchored) . parseIgnoreRule) "a/b" `shouldBe` Just True

    it "strips unescaped trailing spaces" $
        parseIgnoreRule "foo   " `shouldBe` parseIgnoreRule "foo"

    it "keeps a trailing space escaped by a backslash" $
        parseIgnoreRule "foo\\ " `shouldBe` Just (litRule "foo ")

    it "collapses a metacharacter-free segment to Exact" $
        (fmap (.pattern) . parseIgnoreRule) "foo.ts" `shouldBe` Just (IgnorePattern [Exact "foo.ts"])

    it "parses a lone ** segment as GlobStar" $
        (fmap (.pattern) . parseIgnoreRule) "a/**/b"
            `shouldBe` Just (IgnorePattern [Exact "a", GlobStar, Exact "b"])

    it "treats an unterminated bracket as a literal" $
        parseIgnoreRule "a[b" `shouldBe` Just (litRule "a[b")
  where
    litRule t =
        IgnoreRule {pattern = IgnorePattern [Exact t], negated = False, dirOnly = False, anchored = False}

matchingSpec :: Spec
matchingSpec = describe "matching" $ do
    it "matches a slash-free pattern at any depth" $ do
        let gi = rootOnly "foo.ts"
        (ignoresFile gi "foo.ts", ignoresFile gi "a/b/foo.ts") `shouldBe` (True, True)

    it "matches an anchored pattern only at the scope root" $ do
        let gi = rootOnly "/foo.ts"
        (ignoresFile gi "foo.ts", ignoresFile gi "a/foo.ts") `shouldBe` (True, False)

    it "does not let a directory-only pattern match a file" $
        ignoresFile (rootOnly "logs/") "logs" `shouldBe` False

    it "lets a directory-only pattern match a directory" $
        ignoresDir (rootOnly "logs/") "logs" `shouldBe` True

    it "ignores everything beneath an ignored directory" $
        ignoresFile (rootOnly "logs/") "logs/a/b.ts" `shouldBe` True

    it "does not let * cross a separator" $
        ignoresFile (rootOnly "/a*b") "a/b" `shouldBe` False

    it "matches a single character with ?" $ do
        let gi = rootOnly "a?c"
        (ignoresFile gi "abc", ignoresFile gi "ac") `shouldBe` (True, False)

    it "matches a character class" $ do
        let gi = rootOnly "a[bc]d"
        (ignoresFile gi "abd", ignoresFile gi "aed") `shouldBe` (True, False)

    it "matches a negated character class" $ do
        let gi = rootOnly "a[!bc]d"
        (ignoresFile gi "aed", ignoresFile gi "abd") `shouldBe` (True, False)

    it "matches a POSIX character class" $ do
        let gi = rootOnly "a[[:digit:]]c"
        (ignoresFile gi "a5c", ignoresFile gi "abc") `shouldBe` (True, False)

    it "treats **/x as equivalent to x" $ do
        let gi = rootOnly "**/foo.ts"
        (ignoresFile gi "foo.ts", ignoresFile gi "a/b/foo.ts") `shouldBe` (True, True)

    it "matches everything inside a trailing /**, but not the directory itself" $ do
        let gi = rootOnly "a/**"
        (ignoresFile gi "a/b.ts", ignoresDir gi "a") `shouldBe` (True, False)

    it "lets a/**/b span zero or more directories" $ do
        let gi = rootOnly "a/**/b"
        (ignoresFile gi "a/b", ignoresFile gi "a/x/y/b") `shouldBe` (True, True)

    it "lets the last matching line win" $
        ignoresFile (rootOnly "*.ts\n!keep.ts") "keep.ts" `shouldBe` False

    it "lets a re-negation win again" $
        ignoresFile (rootOnly "*.ts\n!keep.ts\nkeep.ts") "keep.ts" `shouldBe` True

scopeSpec :: Spec
scopeSpec = describe "scopes" $ do
    it "lets a deeper .gitignore override a shallower one" $ do
        let gi = gitIgnoreOf [("", "*.log"), ("src", "!*.log")]
        (ignoresFile gi "a.log", ignoresFile gi "src/a.log") `shouldBe` (True, False)

    it "confines a rule to its own subtree" $ do
        let gi = gitIgnoreOf [("src", "*.log")]
        (ignoresFile gi "src/a.log", ignoresFile gi "other/a.log") `shouldBe` (True, False)

    it "anchors a nested rule to its own directory, not the project root" $ do
        let gi = gitIgnoreOf [("src", "/a.ts")]
        (ignoresFile gi "src/a.ts", ignoresFile gi "src/sub/a.ts") `shouldBe` (True, False)

alwaysIgnoredSpec :: Spec
alwaysIgnoredSpec = describe "always-ignored directories" $ do
    it "ignores node_modules regardless of any rule" $
        ignoresDir (rootOnly "!node_modules") "node_modules" `shouldBe` True

    it "ignores files beneath an always-ignored directory" $
        ignoresFile (rootOnly "") "node_modules/pkg/index.ts" `shouldBe` True

    it "matches an always-ignored name exactly, not as a prefix" $
        alwaysIgnored Entry {path = inProject "node_modules_backup", isDir = True} `shouldBe` False

orderingProps :: Spec
orderingProps = describe "ordering and negation laws" $ do
    prop "negating a rule un-ignores everything it ignored" $ do
        line <- forAll genPositiveLine
        query <- forAll genQuery
        let ignored = ignoresQuery (rootOnly line) query
        let unIgnored = ignoresQuery (rootOnly (line <> "\n!" <> line)) query
        (ignored, ignored && unIgnored) === (ignored, False)

    prop "re-asserting after a negation ignores again" $ do
        line <- forAll genPositiveLine
        query <- forAll genQuery
        let ignored = ignoresQuery (rootOnly line) query
        let reIgnored = ignoresQuery (rootOnly (T.intercalate "\n" [line, "!" <> line, line])) query
        reIgnored === ignored

    prop "appending a positive rule never un-ignores a path" $ do
        line <- forAll genPositiveLine
        extra <- forAll genPositiveLine
        query <- forAll genQuery
        let ignored = ignoresQuery (rootOnly line) query
        let stillIgnored = ignoresQuery (rootOnly (line <> "\n" <> extra)) query
        (ignored, ignored && not stillIgnored) === (ignored, False)

    prop "a deeper scope overrides a shallower one" $ do
        line <- forAll genUnanchoredLine
        name <- forAll genName
        -- Git cannot re-include anything beneath an excluded directory, so the
        -- law only holds while src itself escapes the shallower rule.
        when (ignoresDir (rootOnly line) "src") discard
        let gi = gitIgnoreOf [("", line), ("src", "!" <> line)]
        ignoresQuery gi ("src/" <> name) === False

anchoringProps :: Spec
anchoringProps = describe "anchoring and shape laws" $ do
    prop "an anchored pattern never matches below its scope" $ do
        name <- forAll genName
        prefix <- forAll genRelPath
        -- Were the prefix's own first component `name`, the rule would match
        -- that ancestor and ignore the whole subtree along with it.
        when (viaNonEmpty head (T.splitOn "/" prefix) == Just name) discard
        ignoresFile (rootOnly ("/" <> name)) (prefix <> "/" <> name) === False

    prop "a slash-free pattern matches at every depth" $ do
        name <- forAll genName
        prefix <- forAll genRelPath
        let gi = rootOnly name
        ignoresFile gi (prefix <> "/" <> name) === ignoresFile gi name

    prop "a directory-only pattern never matches a file" $ do
        line <- forAll genUnanchoredLine
        name <- forAll genName
        -- Top-level, so that the entry itself is the only thing that can match:
        -- at depth, a dir-only rule legitimately ignores a file via its parent.
        ignoresFile (rootOnly (line <> "/")) name === False

    prop "**/x is equivalent to x" $ do
        name <- forAll genName
        query <- forAll genQuery
        ignoresQuery (rootOnly ("**/" <> name)) query === ignoresQuery (rootOnly name) query

    prop "comments and blank lines never change a verdict" $ do
        lns <- forAll (Gen.list (Range.linear 0 4) genPositiveLine)
        query <- forAll genQuery
        let noise = T.intercalate "\n" (concatMap (\l -> ["# note", "", l]) lns)
        ignoresQuery (rootOnly noise) query === ignoresQuery (rootOnly (T.intercalate "\n" lns)) query

lexerProps :: Spec
lexerProps = describe "lexer laws" $ do
    prop "parse . render is the identity on canonical rules" $ do
        rule <- forAll genIgnoreRule
        parseIgnoreRule (renderIgnoreRule rule) === Just rule

    prop "unescaped trailing spaces are insignificant" $ do
        line <- forAll genPositiveLine
        n <- forAll (Gen.int (Range.linear 1 4))
        parseIgnoreRule (line <> T.replicate n " ") === parseIgnoreRule line

    prop "an escaped trailing space is significant" $ do
        line <- forAll genPositiveLine
        parseIgnoreRule (line <> "\\ ") /== parseIgnoreRule line

differentialProps :: Spec
differentialProps = describe "differential against git check-ignore" $ do
    prop "agrees with git on a single root .gitignore" $ do
        lns <- forAll (Gen.list (Range.linear 1 5) genPositiveLine)
        tree <- forAll genTree
        assertAgreesWithGit [("", T.unlines lns)] tree

    prop "agrees with git on nested .gitignore files" $ do
        rootLines <- forAll (Gen.list (Range.linear 1 4) genPositiveLine)
        srcLines <- forAll (Gen.list (Range.linear 1 4) genPositiveLine)
        tree <- forAll (genTreeUnder "src")
        assertAgreesWithGit [("", T.unlines rootLines), ("src", T.unlines srcLines)] tree
  where
    genTree = mkTree <$> Gen.list (Range.linear 1 6) genRelPath
    genTreeUnder prefix = mkTree . fmap ((prefix <> "/") <>) <$> Gen.list (Range.linear 1 6) genRelPath

    assertAgreesWithGit scopes tree = do
        theirs <- liftIO (gitIgnoredSet scopes tree)
        let gi = gitIgnoreOf scopes
        let ours = Set.fromList $ filter (ignoresDir gi) tree.dirs <> filter (ignoresFile gi) tree.files
        ours === theirs
