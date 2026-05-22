module Monetization.CIDetection (
    detectEnv,
) where

import Effectful (Eff, (:>))
import Effects.System (System, sIsTerminal, sLookupEnv)
import Utils (firstJustM)

-- | The detection categories based on heuristic certainty.
data DetectedEnv
    = -- | 100% Confirmed CI (Cannot run without Pro key)
      CI
    | -- | Suspicious environment (Requires interactive prompt)
      MaybeCI
    | -- | Verified safe local environment
      Terminal
    deriving (Show, Eq)

detectEnv :: (System :> es) => Eff es DetectedEnv
detectEnv = fromMaybe MaybeCI <$> firstJustM id heuristics
  where
    heuristics =
        [ ciHeuristic
        , terminalHeuristic
        ]

    ciHeuristic = do
        isCI <- anyM (fmap isJust . sLookupEnv) ciVars
        if isCI
            then
                pure $ Just CI
            else do
                termEnv <- sLookupEnv "TERM"
                -- Many CI systems or background schedulers use TERM=dumb
                pure $
                    if termEnv == Just "dumb"
                        then Just CI
                        else Nothing

    terminalHeuristic = do
        isTerminal <- sIsTerminal
        if isTerminal then pure $ Just Terminal else pure Nothing

ciVars :: [String]
ciVars =
    [ "CI"
    , "GITHUB_ACTIONS"
    , "GITLAB_CI"
    , "TRAVIS"
    , "CIRCLECI"
    , "JENKINS_URL"
    , "BUILD_ID"
    , "BITBUCKET_COMMIT"
    ]
