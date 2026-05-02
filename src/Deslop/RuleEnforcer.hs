module Deslop.RuleEnforcer (enforceRulebooks) where

import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleGraph, findKnownPath, moduleExists, reachableFrom)
import Deslop.GlobPlus (CompiledRulePattern, CompiledTargetPattern, MatchEnv, matchRule, matchTarget, moduleFromGlob)
import Deslop.Problem (Problem (..))
import Deslop.Rulebook (Forbidden (..), Rule (..), RuleId (..), Rulebook (..), RulebookId (..))
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
            , description = desc
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
        for_ rule.forbidden (traverse_ (executeForbidden m env))
        for_ rule.exists (traverse_ (executeExists m env))

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

executeForbidden ::
    ( Reader ModuleGraph :> es
    , Reader ReachableModules :> es
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    ) =>
    AstModule -> MatchEnv -> Forbidden -> Eff es ()
executeForbidden m env (ForbiddenImport target transitive)
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
                importRaw hop = (T.strip . (.rawStatement)) <$> find (\n -> n.target == hop) m.nodes
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
executeForbidden _ _ (ForbiddenFunctionCall _) = todo

executeExists ::
    ( Reader ModuleGraph :> es
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    , Error DeslopError :> es
    ) =>
    AstModule -> MatchEnv -> CompiledRulePattern -> Eff es ()
executeExists m env pat = do
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
