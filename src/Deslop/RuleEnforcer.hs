module Deslop.RuleEnforcer (enforceRulebooks) where

import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleGraph, findKnownPath, moduleExists, reachableFrom)
import Deslop.GlobPlus (MatchEnv, Segments, hydrate, matchExclude, matchResolved, matchTarget, moduleFromGlob, renderClausePattern, segmentsOf)
import Deslop.GlobPlus.Compiler (interpolate)
import Deslop.Problem (Problem (..), ViolationKind (..))
import Deslop.Rulebook (AllowsClause (..), ExistsClause (..), ForbidsClause (..), Rule (..), RuleId (..), Rulebook (..), RulebookId (..), UsesClause (..))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Effects.ReportProblem (ReportProblem, report)
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)
import Types (DeslopError (..))

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
            , badModule = m.id
            , prose = interpolate env rule.description
            , kind = violationKind
            , fix = interpolate env rule.fix
            }

{- | Every path this module will be tested against, each split into segments
exactly once. A module id is matched against every rule and every clause, so
taking it apart per match is work done as many times as there are clauses.
-}
data Candidates = Candidates
    { self :: Segments
    , imports :: [(AstNode, Segments)]
    , reachable :: [(ModuleId, Segments)]
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
    reachable <- reachableFrom m.id
    let candidates =
            Candidates
                { self = segmentsOf m.id.text
                , imports = [(node, segmentsOf node.target.text) | node <- m.nodes]
                , reachable = [(moduleId, segmentsOf moduleId.text) | moduleId <- reachable]
                }
    runReader candidates $ traverse_ (enforceRulebook m) rulebooks

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

isTarget :: Segments -> Rule -> Maybe MatchEnv
isTarget moduleSegments rule = case matchTarget rule.target moduleSegments of
    Just env | not isExcluded -> Just env
    _ -> Nothing
  where
    isExcluded = any (`matchExclude` moduleSegments) (foldMap toList rule.exclude)

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
        isAllowed segments = any (`matchResolved` segments) allowed
    if transitive
        then traverse_ (transitiveForbiddenImport forbidden isAllowed) candidates.reachable
        else traverse_ (directForbiddenImport forbidden isAllowed) candidates.imports
  where
    directForbiddenImport forbidden isAllowed (ImportNode t rawStatement, segments)
        | matchResolved forbidden segments && not (isAllowed segments) =
            report
                =<< ruleViolation
                    env
                    m
                    DirectImport {imported = t, importStatement = T.strip rawStatement}
        | otherwise = pure ()

    transitiveForbiddenImport forbidden isAllowed (reachableModuleId, segments)
        | matchResolved forbidden segments && not (isAllowed segments) = do
            p <- findKnownPath m.id reachableModuleId
            let firstHop = listToMaybe . drop 1 . toList $ p
                importRaw hop = T.strip . (.rawStatement) <$> find (\n -> n.target == hop) m.nodes
            report
                =<< ruleViolation
                    env
                    m
                    TransitiveImport
                        { chain = p
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
            | transitive = any (matchResolved required . snd) candidates.reachable
            | otherwise = any (matchResolved required . snd) candidates.imports
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
    mid <- case moduleFromGlob env pat of
        Just t -> pure (moduleIdUnsafe t)
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
    exists <- moduleExists mid
    unless exists $
        report =<< ruleViolation env m MissingModule {requiredModule = mid}
