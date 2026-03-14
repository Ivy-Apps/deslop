module Deslop.RuleBookFixtures (
    defaultRuleBookDto,
    defaultRuleDto,
    defaultForbiddenDto,
    nameL,
    rulesL,
    forbiddenL,
    forbiddenImportDto,
)
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Deslop.RuleBook (
    ForbiddenDto (..),
    GlobDto (..),
    RuleBookDto (..),
    RuleDto (..),
    RuleId (..),
    forbiddenL,
    nameL,
    rulesL,
 )

defaultForbiddenDto :: ForbiddenDto
defaultForbiddenDto = ForbiddenImportDto (GlobDto "react") Nothing

defaultRuleDto :: RuleDto
defaultRuleDto =
    RuleDto
        { id = RuleId (T.pack "r1")
        , description = Nothing
        , target = GlobDto "*.ts" :| []
        , exclude = Nothing
        , forbidden = Just [defaultForbiddenDto]
        }

defaultRuleBookDto :: RuleBookDto
defaultRuleBookDto =
    RuleBookDto
        { name = "Test"
        , rules = [defaultRuleDto]
        }

forbiddenImportDto :: String -> Maybe Bool -> ForbiddenDto
forbiddenImportDto glob = ForbiddenImportDto (GlobDto glob)
