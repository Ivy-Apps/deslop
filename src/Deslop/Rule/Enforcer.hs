module Deslop.Rule.Enforcer (enforceRulebooks) where

import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..), ModuleName (..), canonicalName, moduleNameUnsafe)
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
    MatchEnv -> AstModule -> ViolationKind -> Eff es Problem
ruleViolation env m violationKind = do
    rbId <- ask @RulebookId
    rule <- ask @Rule
    pure $
        RuleViolation
            { rulebook = rbId
            , rule = rule.id
            , badModule = canonicalName m
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
    { node :: AstNode
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
    AstModule -> Eff es ()
enforceRulebooks m = do
    rulebooks <- ask @[Rulebook]
    reached <- reachableFrom (InternalKey m.id)
    imported <- traverse importCandidate m.nodes
    let candidates =
            Candidates
                { self = segmentsOfName <$> m.names
                , imports = imported
                , reachable = [ReachCandidate {ref = r, segments = segmentsOfRef r} | r <- reached]
                }
    runReader candidates $ traverse_ (enforceRulebook m) rulebooks
  where
    importCandidate node = do
        let key = graphKeyOf node
        found <- refOfKey key
        let r = fromMaybe ModuleRef {key = key, names = node.specifier :| []} found
        pure ImportCandidate {node = node, ref = r, segments = segmentsOfRef r}

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
    AstModule -> Rulebook -> Eff es ()
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
    AstModule -> Rule -> Eff es ()
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
    AstModule -> MatchEnv -> ForbidsClause -> Eff es ()
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
                        , edge = candidate.node.kind
                        , importStatement = T.strip candidate.node.rawStatement
                        }
        | otherwise = pure ()

    transitiveForbiddenImport breaks candidate
        | breaks candidate.segments = do
            p <- findKnownPath (InternalKey m.id) candidate.ref.key
            let firstHop = listToMaybe . drop 1 . toList $ p
                -- Found by what the edge resolved to, not by the text it was
                -- written with: one module answers to several names.
                importRaw hop = T.strip . (.rawStatement) <$> find ((== hop.key) . graphKeyOf) m.nodes
            report
                =<< ruleViolation
                    env
                    m
                    TransitiveImport
                        { chain = refName <$> p
                        , firstImport = firstHop >>= importRaw
                        , alsoReached = []
                        }
        | otherwise = pure ()

enforceUses ::
    ( Reader RulebookId :> es
    , Reader Rule :> es
    , Reader Candidates :> es
    , ReportProblem :> es
    ) =>
    AstModule -> MatchEnv -> UsesClause -> Eff es ()
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
    AstModule -> MatchEnv -> ExistsClause -> Eff es ()
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
