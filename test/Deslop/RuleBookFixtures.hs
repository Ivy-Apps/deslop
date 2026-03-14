module Deslop.RuleBookFixtures where

import Control.Lens (Lens')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Deslop.RuleBook

defaultRuleBookDto :: RuleBookDto
defaultRuleBookDto =
    RuleBookDto
        { name = "Test"
        , rules = [defaultRuleDto]
        }

defaultRuleDto :: RuleDto
defaultRuleDto =
    RuleDto
        { id = RuleId (T.pack "r1")
        , description = Nothing
        , target = GlobDto "*.ts" :| []
        , exclude = Nothing
        , forbidden = Just [defaultForbiddenDto]
        }

defaultForbiddenDto :: ForbiddenDto
defaultForbiddenDto = ForbiddenImportDto (GlobDto "react") Nothing

-- Lenses for RuleBookDto / RuleDto (disambiguate from RuleBook / Rule same-named fields).
nameL :: Lens' RuleBookDto Text
nameL f (RuleBookDto n r) = fmap (`RuleBookDto` r) (f n)

rulesL :: Lens' RuleBookDto [RuleDto]
rulesL f (RuleBookDto n r) = fmap (RuleBookDto n) (f r)

forbiddenL :: Lens' RuleDto (Maybe [ForbiddenDto])
forbiddenL f (RuleDto i d t e forb) = fmap (RuleDto i d t e) (f forb)

forbiddenImportDto :: String -> Maybe Bool -> ForbiddenDto
forbiddenImportDto glob = ForbiddenImportDto (GlobDto glob)
