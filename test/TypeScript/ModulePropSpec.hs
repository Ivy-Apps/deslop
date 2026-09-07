{- | The property suite for the seam between TypeScript and the core.

P13-P15 are about the line an edge reports. It is derived rather than lexed, by
counting newlines across a token stream that renders back to the source byte for
byte, so P13 checks the answer against the file itself rather than against the
same sum done twice. P15 is the one that guards a user's Baseline: a line is for
the reader and must never reach a Problem Id, or adding a blank line at the top
of a file would unsuppress everything below it.

P16 and P17 are about what a name may be. A Module Name is matched by Rulebook
patterns and written into a committed Baseline, so it has to mean the same thing
to every reader on every machine - which a relative specifier does not, and a
path naming this machine does not.
-}
module TypeScript.ModulePropSpec (spec) where

import Data.Graph (vertices)
import Data.Text qualified as T
import Deslop.CodeGraph (ModuleGraph (..), ModuleNode (..), buildModuleGraph)
import Deslop.Module (
    DependencyEdge (..),
    EdgeTarget (..),
    Location (..),
    Module (..),
    ModuleName (..),
    Specifier (..),
    moduleNameUnsafe,
 )
import Deslop.Problem (ProblemId (..), problemId)
import Deslop.Problem.Baseline (emptyBaseline)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import FileSystem.Path (AbsPath (..), ProjectRoot (..), decodeOsPath, portablePath)
import Generators.TypeScript.Project (
    Naming (..),
    Project,
    Spelling (..),
    asRelative,
    genProject,
    projectAsts,
    projectConfig,
    projectFiles,
    projectRoot,
    renderProject,
    secondaryAlias,
 )
import Hedgehog (Gen, PropertyT, annotate, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import TestUtils (prop)
import TypeScript.CST (TsProgram (..))
import TypeScript.Lint.RelativeSpecifiers (noRelativeSpecifiers)
import TypeScript.Parser (TsFile (..), parseTs)

spec :: Spec
spec = describe "TypeScript.Module properties" $ do
    prop "P13 an edge's line is the line of the file its statement is written on" $ do
        project <- forAll genProject
        spelling <- forAll genSpelling
        let sources = renderProject spelling project
        modules <- lower project sources
        for_ (zip sources modules) $ \((_, source), m) ->
            for_ m.edges $ \edge -> do
                annotate . toString $ "statement: " <> edge.location.code
                lineAt source edge.location.line === Just (firstLineOf edge.location.code)

    prop "P14 prepending newlines shifts every line by exactly that many" $ do
        project <- forAll genProject
        spelling <- forAll genSpelling
        n <- forAll $ Gen.int (Range.linear 0 5)
        let sources = renderProject spelling project
        original <- lower project sources
        shifted <- lower project (prependNewlines n sources)
        edgeLines shifted === map (+ n) (edgeLines original)

    prop "P15 prepending newlines changes no Problem Id" $ do
        project <- forAll genProject
        n <- forAll $ Gen.int (Range.linear 1 5)
        -- Relative specifiers, so that every statement is one the lint pass
        -- reports and there are Ids to compare in the first place.
        let sources = renderProject asRelative project
        original <- lintIds project sources
        shifted <- lintIds project (prependNewlines n sources)
        shifted === original

    prop "P16 no Module Name reaching the graph is relative" $ do
        project <- forAll genProject
        spelling <- forAll genSpelling
        modules <- lower project (renderProject spelling project)
        -- Leaving one module out models one Deslop resolved but never scanned:
        -- a gitignored file, or one outside the tree it was pointed at. Its
        -- vertex must still be named, and named by something that means the
        -- same to every reader.
        for_ (leaveOneOut modules) $ \scanned ->
            for_ (namesIn (buildModuleGraph scanned)) $ \name -> do
                annotate . toString $ "name: " <> name.text
                isRelative name === False

    prop "P17 no name or path a module is lowered with names this machine" $ do
        project <- forAll genProject
        spelling <- forAll genSpelling
        modules <- lower project (renderProject spelling project)
        for_ modules $ \m -> do
            annotate . toString $ "module: " <> portablePath m.path
            namesTheMachine (portablePath m.path) === False
            for_ (toList m.names <> resolvedNames m) $ \name -> do
                annotate . toString $ "name: " <> name.text
                namesTheMachine name.text === False

-- | Every way a dependency can be written down, aliased or relative.
genSpelling :: Gen Spelling
genSpelling = do
    reExport <- Gen.bool
    directory <- Gen.bool
    naming <- Gen.element [Aliased "@/", Aliased secondaryAlias, Relative]
    pure Spelling {reExport = reExport, directory = directory, naming = naming}

prependNewlines :: Int -> [(AbsPath, Text)] -> [(AbsPath, Text)]
prependNewlines n sources = [(p, T.replicate n "\n" <> c) | (p, c) <- sources]

{- | The 1-based line of a source, without the indentation a statement's own
text excludes.
-}
lineAt :: Text -> Int -> Maybe Text
lineAt source n = T.stripStart <$> T.lines source !!? (n - 1)

firstLineOf :: Text -> Text
firstLineOf = fromMaybe "" . viaNonEmpty head . T.lines

edgeLines :: [Module] -> [Int]
edgeLines modules = [e.location.line | m <- modules, e <- m.edges]

-- | Every name of every vertex, however that vertex came to be one.
namesIn :: ModuleGraph -> [ModuleName]
namesIn mg = foldMap (namesOf . nodeAt) (vertices mg.graph)
  where
    nodeAt v = let (node, _, _) = mg.nodeFromV v in node
    namesOf (ParsedModule m) = toList m.names
    namesOf (UnscannedModule names) = toList names
    -- A relative specifier always resolves to some path, so this should never
    -- hold a relative one either. Checked rather than assumed.
    namesOf (ExternalModule written) = [moduleNameUnsafe written.text]

{- | The project as Deslop would see it with one module left unscanned, once per
module. Empty for a single-module project, which has nothing to leave out.
-}
leaveOneOut :: [Module] -> [[Module]]
leaveOneOut modules
    | length modules < 2 = []
    | otherwise = [take i modules <> drop (i + 1) modules | i <- [0 .. length modules - 1]]

isRelative :: ModuleName -> Bool
isRelative name =
    any (`T.isPrefixOf` name.text) ["./", "../"] || name.text `elem` [".", ".."]

-- | Whether text carries the absolute path this project happens to sit at.
namesTheMachine :: Text -> Bool
namesTheMachine t = root `T.isInfixOf` t
  where
    ProjectRoot p = projectRoot
    root = decodeOsPath p.osPath

-- | The names the frontend minted for whatever each edge resolved to.
resolvedNames :: Module -> [ModuleName]
resolvedNames m = [n | e <- m.edges, n <- namesOfTarget e.target]
  where
    namesOfTarget (Resolved _ names) = toList names
    namesOfTarget (External _) = []

lower :: Project -> [(AbsPath, Text)] -> PropertyT IO [Module]
lower project = liftIO . projectAsts project

-- | Every Problem Id the lint pass reports over a whole project.
lintIds :: Project -> [(AbsPath, Text)] -> PropertyT IO [Text]
lintIds project sources =
    liftIO
        . runEff
        . runMockRoFileSystem (mockFiles (projectFiles project))
        . runReportProblem
        . runReader projectConfig
        . runReader projectRoot
        . runReader emptyBaseline
        $ do
            traverse_ (noRelativeSpecifiers . program) sources
            sort . map ((.text) . problemId) <$> getProblems
  where
    program (path, content) = case parseTs TsFile {path = path, content = content} of
        Left err -> TsModule {path = path, cst = error (toText err)}
        Right prog -> prog
