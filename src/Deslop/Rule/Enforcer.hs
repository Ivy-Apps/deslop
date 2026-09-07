module Deslop.Rule.Enforcer (enforceRulebooks) where

import Data.Text qualified as T
import Deslop.CodeGraph (
    GraphKey (..),
    ModuleGraph,
    ModuleRef (..),
    findKnownPath,
    graphKeyOf,
    moduleExists,
    reachableFrom,
    refName,
    refOfKey,
 )
import Deslop.Error (DeslopError (..))
import Deslop.GlobPlus (MatchEnv, ResolvedClause, Segments, hydrate, matchExclude, matchResolved, matchTarget, moduleFromGlob, renderClausePattern, segmentsOf)
import Deslop.GlobPlus.Compiler (interpolate)
import Deslop.Module (
    DependencyEdge (..),
    Location (..),
    Module (..),
    ModuleName (..),
    Specifier (..),
    canonicalName,
    moduleNameUnsafe,
 )
import Deslop.Problem (Problem (..), ViolationKind (..))
import Deslop.Rule.Book (AllowsClause (..), ExistsClause (..), ForbidsClause (..), Rule (..), RuleId (..), Rulebook (..), RulebookId (..), UsesClause (..))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Effects.ReportProblem (ReportProblem, report)

{- | The rule's own prose speaks about the match that violated it, so the
variables its target captured are substituted into it before it is reported.
-}
ruleViolation ::
    ( Reader RulebookId :> es
    , Reader Rule :> es
    ) =>
    MatchEnv -> Module -> ViolationKind -> Eff es Problem
ruleViolation env m violationKind = do
    rbId <- ask @RulebookId
    rule <- ask @Rule
    pure $
        RuleViolation
            { rulebook = rbId
            , rule = rule.id
            , badModule = canonicalName m
            , modulePath = m.path
            , prose = interpolate env rule.description
            , kind = violationKind
            , fix = interpolate env rule.fix
            }

{- | Every path this module will be tested against, each split into segments
exactly once. A module is matched against every rule and every clause, so
taking its names apart per match is work done as many times as there are
clauses.

A module answers to more than one name, so each candidate carries all of them:
a pattern matches the module when it matches any one.
-}
data Candidates = Candidates
    { self :: NonEmpty Segments
    , imports :: [ImportCandidate]
    , reachable :: [ReachCandidate]
    }

data ImportCandidate = ImportCandidate
    { edge :: DependencyEdge
    , ref :: ModuleRef
    , segments :: [Segments]
    }

data ReachCandidate = ReachCandidate
    { ref :: ModuleRef
    , segments :: [Segments]
    }

enforceRulebooks ::
    ( Reader [Rulebook] :> es
    , Reader ModuleGraph :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    Module -> Eff es ()
enforceRulebooks m = do
    rulebooks <- ask @[Rulebook]
    reached <- reachableFrom (ModuleKey m.id)
    imported <- traverse importCandidate m.edges
    let candidates =
            Candidates
                { self = segmentsOfName <$> m.names
                , imports = imported
                , reachable = [ReachCandidate {ref = r, segments = segmentsOfRef r} | r <- reached]
                }
    runReader candidates $ traverse_ (enforceRulebook m) rulebooks
  where
    -- Every edge's target has a vertex, so the lookup always finds one; the
    -- fallback exists only so this is total, and names the edge by what the
    -- source wrote, which is the sole thing known without the graph.
    importCandidate edge = do
        let key = graphKeyOf edge
        found <- refOfKey key
        let r = fromMaybe ModuleRef {key = key, names = moduleNameUnsafe edge.specifier.text :| []} found
        pure ImportCandidate {edge = edge, ref = r, segments = segmentsOfRef r}

segmentsOfRef :: ModuleRef -> [Segments]
segmentsOfRef = map segmentsOfName . toList . (.names)

segmentsOfName :: ModuleName -> Segments
segmentsOfName = segmentsOf . (.text)

enforceRulebook ::
    ( Reader ModuleGraph :> es
    , Reader Candidates :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    Module -> Rulebook -> Eff es ()
enforceRulebook m rulebook =
    runReader rulebook.id $
        traverse_ (enforceRule m) rulebook.rules

enforceRule ::
    ( Reader ModuleGraph :> es
    , Reader Candidates :> es
    , Reader RulebookId :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    Module -> Rule -> Eff es ()
enforceRule m rule = do
    candidates <- ask @Candidates
    case isTarget candidates.self rule of
        Just env -> runReader rule $ execute env
        Nothing -> pure ()
  where
    execute ::
        ( Reader RulebookId :> es
        , Reader Rule :> es
        , Reader ModuleGraph :> es
        , Reader Candidates :> es
        , ReportProblem :> es
        , Error DeslopError :> es
        ) =>
        MatchEnv -> Eff es ()
    execute env = do
        for_ rule.forbids (traverse_ (enforceForbids m env))
        for_ rule.exists (traverse_ (enforceExists m env))
        for_ rule.uses (traverse_ (enforceUses m env))

{- | Whether the Rule applies, and what its target captured.

A module matches when any of its names does; an @exclude@ matching any of them
takes the module out entirely, since excluding is how a Rule is silenced. The
captures come from the first name that matched, which is the canonical one
whenever it matches at all.
-}
isTarget :: NonEmpty Segments -> Rule -> Maybe MatchEnv
isTarget names rule
    | any isExcluded names = Nothing
    | otherwise = asum $ matchTarget rule.target <$> names
  where
    isExcluded segments = any (`matchExclude` segments) (foldMap toList rule.exclude)

-- | Whether a hydrated clause matches any name a module answers to.
matchesAny :: ResolvedClause -> [Segments] -> Bool
matchesAny pattern = any (matchResolved pattern)

{- | Clauses are hydrated once per matched target and then run against every
candidate path, rather than resolved afresh for each one.
-}
enforceForbids ::
    ( Reader ModuleGraph :> es
    , Reader Candidates :> es
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    ) =>
    Module -> MatchEnv -> ForbidsClause -> Eff es ()
enforceForbids m env (ForbidsImport target transitive) = do
    candidates <- ask @Candidates
    allowed <- asks @Rule (fmap (hydrate env . (.target)) . foldMap toList . (.allows))
    let forbidden = hydrate env target
        isAllowed segments = any (`matchesAny` segments) allowed
        breaks segments = matchesAny forbidden segments && not (isAllowed segments)
    if transitive
        then traverse_ (transitiveForbiddenImport breaks) candidates.reachable
        else traverse_ (directForbiddenImport breaks) candidates.imports
  where
    directForbiddenImport breaks candidate
        | breaks candidate.segments =
            report
                =<< ruleViolation
                    env
                    m
                    DirectImport
                        { imported = refName candidate.ref
                        , edge = candidate.edge.kind
                        , location = stripped candidate.edge.location
                        }
        | otherwise = pure ()

    transitiveForbiddenImport breaks candidate
        | breaks candidate.segments = do
            p <- findKnownPath (ModuleKey m.id) candidate.ref.key
            let firstHop = listToMaybe . drop 1 . toList $ p
                -- Found by what the edge resolved to, not by the text it was
                -- written with: one module answers to several names.
                edgeInto hop = stripped . (.location) <$> find ((== hop.key) . graphKeyOf) m.edges
            report
                =<< ruleViolation
                    env
                    m
                    TransitiveImport
                        { chain = refName <$> p
                        , firstImport = firstHop >>= edgeInto
                        , alsoReached = []
                        }
        | otherwise = pure ()

enforceUses ::
    ( Reader RulebookId :> es
    , Reader Rule :> es
    , Reader Candidates :> es
    , ReportProblem :> es
    ) =>
    Module -> MatchEnv -> UsesClause -> Eff es ()
enforceUses m env (UsesImport usesPattern transitive) = do
    candidates <- ask @Candidates
    let required = hydrate env usesPattern
        satisfied
            | transitive = any (matchesAny required . (.segments)) candidates.reachable
            | otherwise = any (matchesAny required . (.segments)) candidates.imports
    unless satisfied $
        report
            =<< ruleViolation
                env
                m
                MissingUse
                    { requiredImport = renderClausePattern env usesPattern
                    , transitive = transitive
                    }

enforceExists ::
    ( Reader ModuleGraph :> es
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    Module -> MatchEnv -> ExistsClause -> Eff es ()
enforceExists m env (ExistsModule pat) = do
    name <- case moduleFromGlob env pat of
        Just t -> pure (moduleNameUnsafe t)
        Nothing -> do
            RulebookId rbIdText <- ask @RulebookId
            rule <- ask @Rule
            let RuleId ruleIdText = rule.id
            throwError . InvalidRuleConfig $
                "Rule '"
                    <> ruleIdText
                    <> "' in rulebook '"
                    <> rbIdText
                    <> "': 'exists' patterns must not contain wildcards (* or **)."
    exists <- moduleExists name
    unless exists $
        report =<< ruleViolation env m MissingModule {requiredModule = name}

-- | A quoted statement without the newlines the CST carried around it.
stripped :: Location -> Location
stripped loc = loc {code = T.strip loc.code}
