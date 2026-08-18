{- | Collapses the duplicates a run reports into the one Problem worth reading.

A module that imports one thing it should not typically drags in a whole
subtree of forbidden modules behind it, and every module in that subtree is a
transitive violation of the same Rule. They are all repaired by the same single
edit, so reporting them one by one buries the edit under its own consequences.

Only same-kind duplicates of the same "Deslop.Problem.ProblemId" are collapsed.
A Rule that a module breaks in two different ways - reaching what it must not
and failing to reach what it must - still has both reported.
-}
module Deslop.ProblemShrinker (compactProblems) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Deslop.Problem (Problem (..), ViolationKind (..), problemId)
import TypeScript.ModuleResolver (ModuleId)

{- | One Problem per @(ProblemId, kind)@ for transitive imports, every other
Problem untouched. Idempotent, and it never drops a 'Deslop.Problem.ProblemId'
- so it cannot change what a baseline suppresses.
-}
compactProblems :: [Problem] -> [Problem]
compactProblems problems = sort $ rest <> fmap collapse (Map.elems grouped)
  where
    (rest, transitives) = partitionEithers . fmap classify $ problems
    grouped = Map.fromListWith (<>) [(problemId t.problem, pure t) | t <- transitives]

{- | A transitive-import Violation with the chains that decide its fate lifted
out, so grouping never has to look inside a Problem again. @stands@ is every
chain the Violation speaks for - its own, and any it has already absorbed.
-}
data Transitive = Transitive
    { problem :: Problem
    , chain :: NonEmpty ModuleId
    , stands :: NonEmpty (NonEmpty ModuleId)
    }

classify :: Problem -> Either Problem Transitive
classify p@RuleViolation {kind = TransitiveImport {chain, alsoReached}} =
    Right Transitive {problem = p, chain = chain, stands = chain :| alsoReached}
classify p = Left p

{- | The shortest chain survives and absorbs what the rest stood for. Ties are
broken by the chain itself, which is a total order, so the survivor does not
depend on the order the Rules happened to be enforced in.
-}
collapse :: NonEmpty Transitive -> Problem
collapse ts = absorb (concatMap (toList . (.stands)) losers) winner.problem
  where
    winner :| losers = NE.sortWith (\t -> (NE.length t.chain, t.chain)) ts

{- | Records further chains a Violation now stands in for, keeping the ones it
already did so that compacting an already-compacted report changes nothing. A
no-op on anything that is not a transitive import, which 'classify' never
groups.
-}
absorb :: [NonEmpty ModuleId] -> Problem -> Problem
absorb chains p@RuleViolation {kind = k@TransitiveImport {alsoReached}} =
    p {kind = k {alsoReached = alsoReached <> chains}}
absorb _ p = p
