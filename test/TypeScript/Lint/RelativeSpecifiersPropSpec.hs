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
import Generators.TypeScript.CST (genTsProgram)
import Generators.TypeScript.Project (
    Project,
    Naming (..),
    Spelling (..),
    asImports,
    genProject,
    projectAsts,
    projectConfig,
    projectRoot,
    projectFiles,
    reachability,
    renderProject,
    secondaryAlias,
 )
import Hedgehog (PropertyT, annotate, failure, forAll, (===))
import Renderable (Renderable (render))
import System.OsPath (OsPath)
import Test.Hspec
import TestUtils (ap, prop)
import TypeScript.Config (TsConfig)
import TypeScript.CST (TsNode, TsProgram (..), withSpecifier)
import TypeScript.Lint.RelativeSpecifiers (noRelativeSpecifiers)
import TypeScript.Parser (TsFile (..), parseTs)

spec :: Spec
spec = describe "TypeScript.Lint.RelativeSpecifiers properties" $ do
    prop "P4 fixing an already fixed file changes nothing further" $ do
        project <- forAll genProject
        for_ (aliasedSources project) $ \source -> do
            once <- lintOnce project source
            twice <- lintOnce project (fst source, renderCst once)
            renderCst twice === renderCst once

    prop "P5 a fix changes the specifier and nothing else" $ do
        project <- forAll genProject
        for_ (aliasedSources project) $ \source -> do
            original <- parseSource source
            fixed <- lintOnce project source
            map anonymise fixed === map anonymise original

    prop "P6 a file with nothing to report comes back byte for byte" $ do
        planted <- forAll genTsProgram
        let source = (ap "/home/repo/src/generated.ts", renderCst planted)
        original <- parseSource source
        (fixed, problems) <- lintReporting projectConfig emptyBaseline [] (fst source) original
        when (null problems) $ fixed === original

    prop "P12 the module graph does not depend on what the baseline suppresses" $ do
        project <- forAll genProject
        unsuppressed <- lintedAsts project emptyBaseline
        problems <- concat <$> traverse (fmap snd . lintProblems project emptyBaseline) (aliasedSources project)
        suppressed <- lintedAsts project (baselineOf . map ((.text) . problemId) $ problems)
        reachability suppressed === reachability unsuppressed

{- | Sources written with the alias that is not canonical, so that every
statement in them is something the fixer wants to rewrite.
-}
aliasedSources :: Project -> [(AbsPath, Text)]
aliasedSources = renderProject asImports {naming = Aliased secondaryAlias}

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

-- | Every module of the project, linted under one baseline and then lowered.
lintedAsts :: Project -> Baseline -> PropertyT IO [Module]
lintedAsts project baseline = do
    linted <- traverse lintSource (aliasedSources project)
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
