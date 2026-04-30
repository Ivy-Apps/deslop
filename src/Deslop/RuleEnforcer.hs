module Deslop.RuleEnforcer (enforceRulebooks) where

import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (ModuleGraph)
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

enforceRulebooks ::
    ( Reader [Rulebook] :> es
    , Reader ModuleGraph :> es
    , ReportProblem :> es
    ) =>
    AstModule -> Eff es ()
enforceRulebooks m =
    ask @[Rulebook] >>= traverse_ (enforceRulebook m)

enforceRulebook ::
    ( Reader ModuleGraph :> es
    , ReportProblem :> es
    ) =>
    AstModule -> Rulebook -> Eff es ()
enforceRulebook m rulebook =
    runReader rulebook.id $
        traverse_ (enforceRule m) rulebook.rules

enforceRule ::
    ( Reader ModuleGraph :> es
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
        , ReportProblem :> es
        ) =>
        MatchEnv -> Eff es ()
    execute env = do
        case rule.forbidden of
            Just fs -> traverse_ (executeForbidden m env) fs
            Nothing -> pure ()

isTarget :: ModuleId -> Rule -> (Maybe MatchEnv)
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
    , Reader RulebookId :> es
    , Reader Rule :> es
    , ReportProblem :> es
    ) =>
    AstModule -> MatchEnv -> Forbidden -> Eff es ()
executeForbidden m env (ForbiddenImport target transitive)
    | transitive = todo
    | otherwise = traverse_ directForbiddenImport m.nodes
  where
    directForbiddenImport (ImportNode t)
        | matchRule target env t.text =
            let
                message = "Module '" <> m.id.text <> "' directly imports '" <> t.text <> "'."
             in
                ruleViolation m message
                    >>= report
        | otherwise = pure () -- Nothing to report
executeForbidden _ _ (ForbiddenFunctionCall _) = todo
