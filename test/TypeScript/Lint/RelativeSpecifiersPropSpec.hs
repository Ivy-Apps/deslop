{- | The property suite for the one part of Deslop that writes to somebody's
source, and the reason its lexer is unforgiving.

P4 and P6 say the fixer settles and then stops. P5 is the one that matters: a
run may change a specifier and nothing else, so a statement misread as a
dependency cannot corrupt the file around it.

P12 belongs to the module graph rather than to the fixer, but it can only be
stated here, where the baseline is: accepting a Problem must not change what
Deslop believes the architecture is. It used to, because the graph took its
edges from whatever this pass left behind.
-}
module TypeScript.Lint.RelativeSpecifiersPropSpec (spec) where

import Deslop.Module (Module)
import Deslop.Problem (Problem, ProblemId (..), problemId)
import Deslop.Problem.Baseline (Baseline, emptyBaseline)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import FileSystem.Path (AbsPath)
import Fixtures.Deslop.Problem.Baseline (baselineOf)
import Generators.TypeScript.Project (
    Project,
    Spelling (..),
    asImports,
    asRelative,
    genProject,
    projectAsts,
    projectConfig,
    projectFiles,
    projectRoot,
    reachability,
    renderProject,
 )
import Hedgehog (PropertyT, annotate, cover, failure, forAll, (===))
import Renderable (Renderable (render))
import System.OsPath (OsPath)
import Test.Hspec
import TestUtils (prop)
import TypeScript.Config (TsConfig)
import TypeScript.CST (TsNode, TsProgram (..), withSpecifier)
import TypeScript.Lint.RelativeSpecifiers (noRelativeSpecifiers)
import TypeScript.Parser (TsFile (..), parseTs)

spec :: Spec
spec = describe "TypeScript.Lint.RelativeSpecifiers properties" $ do
    prop "P4 fixing an already fixed file changes nothing further" $ do
        project <- forAll genProject
        rewrote <- forM (relativeSources project) $ \source -> do
            once <- lintOnce project source
            twice <- lintOnce project (fst source, renderCst once)
            renderCst twice === renderCst once
            pure (renderCst once /= snd source)
        coverRewriting rewrote

    prop "P5 a fix changes the specifier and nothing else" $ do
        project <- forAll genProject
        rewrote <- forM (relativeSources project) $ \source -> do
            original <- parseSource source
            fixed <- lintOnce project source
            map anonymise fixed === map anonymise original
            pure (renderCst fixed /= renderCst original)
        coverRewriting rewrote

    prop "P6 a file that resolves and has nothing to report comes back byte for byte" $ do
        project <- forAll genProject
        quiet <- forM (renderProject asImports project) $ \source -> do
            original <- parseSource source
            (fixed, problems) <-
                lintReporting projectConfig emptyBaseline (projectFiles project) (fst source) original
            when (null problems) $ fixed === original
            pure (null problems)
        cover 80 "resolved with nothing to report" (and quiet)

    prop "P12 the module graph does not depend on what the baseline suppresses" $ do
        project <- forAll genProject
        for_ fixableSpellings $ \spelling -> do
            let sources = renderProject spelling project
            unsuppressed <- lintedAsts project sources emptyBaseline
            problems <- concat <$> traverse (fmap snd . lintProblems project emptyBaseline) sources
            suppressed <- lintedAsts project sources . baselineOf . map ((.text) . problemId) $ problems
            reachability suppressed === reachability unsuppressed
            cover 60 "a problem was baselined" (not . null $ problems)

{- | Sources written with relative specifiers, so that every statement in them
is one the fixer wants to rewrite. An aliased spelling is not: one module
answers to several names, so @~\/server\/db@ where @\@\/server\/db@ is canonical
is the same module named another way and the rule leaves it alone. Written with
an alias, these properties observed a pass that did nothing.

Both spellings of a dependency, because @deslop fix@ rewrites both and the
re-export grammar is the stricter of the two.
-}
relativeSources :: Project -> [(AbsPath, Text)]
relativeSources project = foldMap (`renderProject` project) fixableSpellings

fixableSpellings :: [Spelling]
fixableSpellings = [asRelative, asRelative {reExport = True}]

{- | That the run under test actually rewrote something.

The defect these properties had was silence: a generator change left them
comparing a pass that did nothing with itself, and they stayed green. A
coverage claim fails when the branch it names goes unvisited, which is the only
way to state "this property reached the code it is about".
-}
coverRewriting :: [Bool] -> PropertyT IO ()
coverRewriting = cover 60 "a specifier was rewritten" . or

-- | A node with its specifier blanked: everything a fix must leave alone.
anonymise :: TsNode -> TsNode
anonymise = withSpecifier ""

renderCst :: [TsNode] -> Text
renderCst = render

lintOnce :: Project -> (AbsPath, Text) -> PropertyT IO [TsNode]
lintOnce project source = fst <$> lintProblems project emptyBaseline source

lintProblems :: Project -> Baseline -> (AbsPath, Text) -> PropertyT IO ([TsNode], [Problem])
lintProblems project baseline source = do
    parsed <- parseSource source
    lintReporting projectConfig baseline (projectFiles project) (fst source) parsed

lintReporting ::
    TsConfig -> Baseline -> [OsPath] -> AbsPath -> [TsNode] -> PropertyT IO ([TsNode], [Problem])
lintReporting cfg baseline files path nodes =
    liftIO
        . runEff
        . runMockRoFileSystem (mockFiles files)
        . runReportProblem
        . runReader cfg
        . runReader projectRoot
        . runReader baseline
        $ do
            result <- noRelativeSpecifiers TsModule {path = path, cst = nodes}
            problems <- getProblems
            pure (result.cst, problems)

{- | Every module of the project, linted under one baseline and then lowered.

One spelling at a time: 'renderProject' keys its output by module path, so
lowering two spellings together would mint two 'Module's for one file and leave
'reachability' collapsing a graph that shadows itself.
-}
lintedAsts :: Project -> [(AbsPath, Text)] -> Baseline -> PropertyT IO [Module]
lintedAsts project sources baseline = do
    linted <- traverse lintSource sources
    liftIO . projectAsts project $ linted
  where
    lintSource source = do
        (nodes, _) <- lintProblems project baseline source
        pure (fst source, renderCst nodes)

parseSource :: (AbsPath, Text) -> PropertyT IO [TsNode]
parseSource (path, content) =
    case parseTs TsFile {path = path, content = content} of
        Left err -> annotate err >> failure
        Right prog -> pure prog.cst
