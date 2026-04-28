module Deslop.RuleEnforcer (enforceRulebooks) where

import Deslop.AST (AstModule (..))
import Deslop.CodeGraph (ModuleGraph)
import Deslop.GlobPlus (CompiledTargetPattern, MatchEnv, matchTarget)
import Deslop.Rulebook (Rule (..), Rulebook, RulebookId)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effects.ReportProblem (ReportProblem)
import TypeScript.ModuleResolver (ModuleId (..))
import Utils (todo)

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
enforceRulebook = todo

enforceRule ::
    ( Reader ModuleGraph :> es
    , ReportProblem :> es
    ) =>
    AstModule -> RulebookId -> Rule -> Eff es ()
enforceRule m rbId rule = case isTarget m.id rule of
    Just env -> execute env
    Nothing -> pure ()
  where
    execute _ = pure ()

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
