{- | Collapses the duplicates a run reports into the one Problem worth reading.

A module that imports one thing it should not typically drags in a whole
subtree of forbidden modules behind it, and every module in that subtree is a
transitive violation of the same Rule. They are all repaired by the same single
edit, so reporting them one by one buries the edit under its own consequences.

A dependency a Rule cannot judge is the same: every target reaching it reports
it, at the module that wrote it, and one report is all the reader needs.

Only same-kind duplicates of the same "Deslop.Problem.ProblemId" are collapsed.
A Rule that a module breaks in two different ways - reaching what it must not
and failing to reach what it must - still has both reported.
-}
module Deslop.Problem.Shrinker (compactProblems) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Deslop.Module (ModuleName)
import Deslop.Problem (Problem (..), ViolationKind (..), problemId)

{- | One Problem per @(ProblemId, kind)@ for transitive and unjudged imports,
every other Problem untouched. Idempotent, and it never drops a 'Deslop.Problem.ProblemId'
- so it cannot change what a baseline suppresses.
-}
compactProblems :: [Problem] -> [Problem]
compactProblems problems = sort $ rest <> fmap collapse (Map.elems grouped)
  where
    (rest, duplicable) = partitionEithers . fmap classify $ problems
    grouped = Map.fromListWith (<>) [(problemId d.problem, pure d) | d <- duplicable]

{- | A Violation that may have duplicates, with the chains that decide its fate
lifted out, so grouping never has to look inside a Problem again. @stands@ is
every chain the Violation speaks for - its own, and any it has already absorbed.
-}
data Duplicable = Duplicable
    { problem :: Problem
    , chain :: NonEmpty ModuleName
    , stands :: NonEmpty (NonEmpty ModuleName)
    }

{- | The two kinds every target or every reached module repeats. An unjudged
import keeps no record of the chains it stood for: the one it shows is an
example of how a target gets there, and the dependency is what is reported.
-}
classify :: Problem -> Either Problem Duplicable
classify p@RuleViolation {kind = TransitiveImport {chain, alsoReached}} =
    Right Duplicable {problem = p, chain = chain, stands = chain :| alsoReached}
classify p@RuleViolation {kind = UnjudgedImport {chain}} =
    Right Duplicable {problem = p, chain = chain, stands = chain :| []}
classify p = Left p

{- | The shortest chain survives and absorbs what the rest stood for. Ties are
broken by the chain itself, which is a total order, so the survivor does not
depend on the order the Rules happened to be enforced in.
-}
collapse :: NonEmpty Duplicable -> Problem
collapse ds = absorb (concatMap (toList . (.stands)) losers) winner.problem
  where
    winner :| losers = NE.sortWith (\d -> (NE.length d.chain, d.chain)) ds

{- | Records further chains a Violation now stands in for, keeping the ones it
already did so that compacting an already-compacted report changes nothing. A
no-op on anything that is not a transitive import.
-}
absorb :: [NonEmpty ModuleName] -> Problem -> Problem
absorb chains p@RuleViolation {kind = k@TransitiveImport {alsoReached}} =
    p {kind = k {alsoReached = alsoReached <> chains}}
absorb _ p = p
