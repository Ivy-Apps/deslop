module Deslop.RuleEnforcer (enforceRulebooks) where

import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleGraph, findKnownPath, moduleExists, reachableFrom)
import Deslop.GlobPlus (CompiledTargetPattern, MatchEnv, matchRule, matchTarget, moduleFromGlob, renderRulePattern)
import Deslop.Problem (Problem (..))
import Deslop.Rulebook (ExistsClause (..), ForbidsClause (..), Rule (..), RuleId (..), Rulebook (..), RulebookId (..), UsesClause (..))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effects.ReportProblem (ReportProblem, report)
import TypeScript.ModuleResolver (ModuleId (..), moduleIdUnsafe)
import Types (DeslopError (..))
import Utils (todo)

ruleViolation ::
    ( Reader RulebookId :> es
    , Reader Rule :> es
    ) =>
    AstModule -> Text -> Eff es Problem
ruleViolation m desc = do
    rbId <- ask @RulebookId
    rule <- ask @Rule
    pure $
        RuleViolation
            { rulebook = rbId
            , rule = rule.id
            , badModule = m.id
            , description = rule.description <> "\n\n" <> desc
            , fix = rule.fix
            }

newtype ReachableModules = ReachableModules [ModuleId]

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
    runReader (ReachableModules reachable) $
        traverse_ (enforceRulebook m) rulebooks

enforceRulebook ::
    ( Reader ModuleGraph :> es
    , Reader ReachableModules :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    AstModule -> Rulebook -> Eff es ()
enforceRulebook m rulebook =
    runReader rulebook.id $
        traverse_ (enforceRule m) rulebook.rules

enforceRule ::
    ( Reader ModuleGraph :> es
    , Reader ReachableModules :> es
    , Reader RulebookId :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    AstModule -> Rule -> Eff es ()
enforceRule m rule = case isTarget m.id rule of
    Just env -> runReader rule $ execute env
    Nothing -> pure ()
  where
    execute ::
        ( Reader RulebookId :> es
        , Reader Rule :> es
        , Reader ModuleGraph :> es
        , Reader ReachableModules :> es
        , ReportProblem :> es
        , Error DeslopError :> es
        ) =>
        MatchEnv -> Eff es ()
    execute env = do
        for_ rule.forbids (traverse_ (executeForbids m env))
        for_ rule.exists (traverse_ (executeExists m env))
        for_ rule.uses (traverse_ (executeUses m env))

isTarget :: ModuleId -> Rule -> Maybe MatchEnv
isTarget moduleId rule = case matchTarget rule.target moduleId.text of
    Just env ->
        if isExcluded (toList <$> rule.exclude)
            then Nothing
            else Just env
    Nothing -> Nothing
  where
    isExcluded :: Maybe [CompiledTargetPattern] -> Bool
    isExcluded Nothing = False
    isExcluded (Just []) = False
    isExcluded (Just (x : xs)) = case matchTarget x moduleId.text of
        Just _ -> True
        Nothing -> isExcluded (Just xs)

executeForbids ::
    ( Reader ModuleGraph :> es
    , Reader ReachableModules :> es
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    ) =>
    AstModule -> MatchEnv -> ForbidsClause -> Eff es ()
executeForbids m env (ForbidsImport target transitive)
    | transitive = do
        ReachableModules reachable <- ask @ReachableModules
        traverse_ transitiveCheck reachable
    | otherwise = traverse_ directForbiddenImport m.nodes
  where
    directForbiddenImport (ImportNode t rawStatement)
        | matchRule target env t.text =
            let
                message =
                    "Module '"
                        <> m.id.text
                        <> "' directly imports '"
                        <> t.text
                        <> "'.\n```ts\n"
                        <> T.strip rawStatement
                        <> "\n```"
             in
                ruleViolation m message
                    >>= report
        | otherwise = pure ()

    transitiveCheck rid
        | matchRule target env rid.text = do
            p <- findKnownPath m.id rid
            let via = " via: " <> T.intercalate " → " (map (.text) (toList p))
                firstHop = listToMaybe (drop 1 (toList p))
                importRaw hop = T.strip . (.rawStatement) <$> find (\n -> n.target == hop) m.nodes
                stmtSuffix = maybe "" (\raw -> "\n```ts\n" <> raw <> "\n```") (firstHop >>= importRaw)
                message =
                    "Module '"
                        <> m.id.text
                        <> "' transitively imports '"
                        <> rid.text
                        <> "'"
                        <> via
                        <> "."
                        <> stmtSuffix
            ruleViolation m message >>= report
        | otherwise = pure ()
executeForbids _ _ (ForbidsFunctionCall _) = todo

executeUses ::
    ( Reader RulebookId :> es
    , Reader Rule :> es
    , Reader ReachableModules :> es
    , ReportProblem :> es
    ) =>
    AstModule -> MatchEnv -> UsesClause -> Eff es ()
executeUses m env (UsesImport usesPattern transitive)
    | transitive = do
        ReachableModules reachable <- ask @ReachableModules
        unless (any (\rid -> matchRule usesPattern env rid.text) reachable) $ do
            let msg =
                    "Module '"
                        <> m.id.text
                        <> "' must transitively import '"
                        <> renderRulePattern env usesPattern
                        <> "'."
            ruleViolation m msg >>= report
    | otherwise = do
        let imports = m.nodes
        unless (any (\node -> matchRule usesPattern env node.target.text) imports) $ do
            let msg =
                    "Module '"
                        <> m.id.text
                        <> "' must import '"
                        <> renderRulePattern env usesPattern
                        <> "'."
            ruleViolation m msg >>= report

executeExists ::
    ( Reader ModuleGraph :> es
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    AstModule -> MatchEnv -> ExistsClause -> Eff es ()
executeExists m env (ExistsModule pat) = do
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
    unless exists $ do
        let msg =
                "Module '"
                    <> m.id.text
                    <> "' requires '"
                    <> mid.text
                    <> "' to exist."
        ruleViolation m msg >>= report
