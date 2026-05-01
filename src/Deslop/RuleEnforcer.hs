module Deslop.RuleEnforcer (enforceRulebooks) where

import Data.Text qualified as T
import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleGraph, findKnownPath, reachableFrom)
import Deslop.GlobPlus (CompiledTargetPattern, MatchEnv, matchRule, matchTarget)
import Deslop.Problem (Problem (..))
import Deslop.Rulebook (Forbidden (..), Rule (..), Rulebook (..), RulebookId)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask, runReader)
import Effects.ReportProblem (ReportProblem, report)
import TypeScript.ModuleResolver (ModuleId (..))
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
        ) =>
        MatchEnv -> Eff es ()
    execute env =
        for_ rule.forbidden (traverse_ (executeForbidden m env))

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
    directForbiddenImport (ImportNode t)
        | matchRule target env t.text =
            let
                message = "Module '" <> m.id.text <> "' directly imports '" <> t.text <> "'."
             in
                ruleViolation m message
                    >>= report
        | otherwise = pure ()

    transitiveCheck rid
        | matchRule target env rid.text = do
            p <- findKnownPath m.id rid
            let via = " via: " <> T.intercalate " → " (map (.text) (toList p))
                message = "Module '" <> m.id.text <> "' transitively imports '" <> rid.text <> "'" <> via <> "."
            ruleViolation m message >>= report
        | otherwise = pure ()
executeForbidden _ _ (ForbiddenFunctionCall _) = todo
