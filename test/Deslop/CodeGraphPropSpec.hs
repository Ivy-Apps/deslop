{- | The property suite for the module graph, and the specification of what
"one file, one vertex" buys.

P7 is the differential test: the graph must agree with the project's own
dependency closure, computed here by brute force from what the project /says/
it depends on rather than from how any of it is written.

P9, P10 and P11 are the three bugs this suite exists for. Each says that
respelling a dependency leaves the architecture alone, and each fails against a
graph keyed by the text an author typed:

* P9  is issue #173 - a re-export is an edge.
* P10 is issue #204 - a barrel is one module, not two.
* P11 is issue #117 - two aliases onto one file are one module.
-}
module Deslop.CodeGraphPropSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Deslop.Module (Module)
import Generators.TypeScript.Project (
    Naming (..),
    Project (..),
    ProjectModule (..),
    Spelling (..),
    asImports,
    asReExports,
    directoryForm,
    genProject,
    indexForm,
    primaryAlias,
    projectAsts,
    reachability,
    renderProject,
    secondaryAlias,
 )
import Hedgehog (PropertyT, forAll, (===))
import Test.Hspec
import TestUtils (prop)

spec :: Spec
spec = describe "Deslop.CodeGraph properties" $ do
    prop "P7 agrees with the project's own dependency closure" $ do
        project <- forAll genProject
        actual <- reachabilityUnder asImports project
        actual === expectedReachability project

    prop "P8 a dependency named by a barrel's directory form lands on the barrel" $ do
        project <- forAll genProject
        actual <- reachabilityUnder directoryForm project
        actual === expectedReachability project

    prop "P9 writing a dependency as a re-export does not change what is reachable" $ do
        project <- forAll genProject
        asImport <- reachabilityUnder asImports project
        asReExport <- reachabilityUnder asReExports project
        asReExport === asImport

    prop "P10 naming a barrel by its directory does not change what is reachable" $ do
        project <- forAll genProject
        byIndex <- reachabilityUnder indexForm project
        byDirectory <- reachabilityUnder directoryForm project
        byDirectory === byIndex

    prop "P11 swapping one alias for another onto the same file changes nothing" $ do
        project <- forAll genProject
        byPrimary <- reachabilityUnder asImports project
        bySecondary <- reachabilityUnder asImports {naming = Aliased secondaryAlias} project
        bySecondary === byPrimary

reachabilityUnder :: Spelling -> Project -> PropertyT IO (Map Text [Text])
reachabilityUnder spelling project = reachability <$> astsUnder spelling project

astsUnder :: Spelling -> Project -> PropertyT IO [Module]
astsUnder spelling project = liftIO . projectAsts project $ renderProject spelling project

{- | What the project says it reaches, by brute force over its declared
dependencies. Includes each module itself, which is what 'Data.Graph.reachable'
reports.
-}
expectedReachability :: Project -> Map Text [Text]
expectedReachability project =
    Map.fromList [(canonical m.name, sort . map canonical . toList . closure $ m.name) | m <- project.modules]
  where
    dependencies = Map.fromList [(m.name, m.deps) | m <- project.modules]

    closure start = walk (Set.singleton start) [start]

    walk seen [] = seen
    walk seen (x : queue) =
        let unseen = filter (`Set.notMember` seen) (Map.findWithDefault [] x dependencies)
         in walk (foldr Set.insert seen unseen) (unseen <> queue)

    canonical name = primaryAlias <> name
