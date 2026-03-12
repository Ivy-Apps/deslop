module Deslop.RuleBookSpec (spec) where

import Data.ByteString.Char8 qualified as B8
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Deslop.RuleBook
    ( ForbiddenDto (ForbiddenImportDto),
      RelativeModuleId (RelativeModuleId),
      RuleBookDto (RuleBookDto),
      RuleDto (RuleDto),
      RuleId (RuleId),
      parseRuleBookYaml
    )
import Test.Hspec

spec :: Spec
spec = do
  describe "parseRuleBookYaml" $ do
    it "parses a rulebook with name, rules, target globs, and forbidden import (import key)" $ do
      let yaml =
            B8.pack
              "name: Demo Rulebook\n\
              \rules:\n\
              \  # P0\n\
              \  - id: no-react-in-core\n\
              \    description: \"The core must not have React dependencies\"\n\
              \    target:\n\
              \      - \"@/client/core/**/*\"\n\
              \      - \"@/server/**/*\"\n\
              \      - \"@/shared/**/*\"\n\
              \    forbidden:\n\
              \      - import: react\n\
              \        transitive: true\n"
      let expected =
            RuleBookDto
              (T.pack "Demo Rulebook")
              [ RuleDto
                  (RuleId (T.pack "no-react-in-core"))
                  (Just (T.pack "The core must not have React dependencies"))
                  ( RelativeModuleId (T.pack "@/client/core/**/*")
                      :| [ RelativeModuleId (T.pack "@/server/**/*"),
                           RelativeModuleId (T.pack "@/shared/**/*")
                         ]
                  )
                  [ ForbiddenImportDto
                      (RelativeModuleId (T.pack "react"))
                      (Just True)
                  ]
              ]
      parseRuleBookYaml yaml `shouldBe` Right expected

    it "parses forbidden entry with target key (alternative to import)" $ do
      let yaml =
            B8.pack
              "name: Minimal\n\
              \rules:\n\
              \  - id: no-lodash\n\
              \    target:\n\
              \      - \"@/shared/**/*\"\n\
              \    forbidden:\n\
              \      - target: lodash\n\
              \        transitive: false\n"
      let expected =
            RuleBookDto
              (T.pack "Minimal")
              [ RuleDto
                  (RuleId (T.pack "no-lodash"))
                  Nothing
                  (RelativeModuleId (T.pack "@/shared/**/*") :| [])
                  [ ForbiddenImportDto
                      (RelativeModuleId (T.pack "lodash"))
                      (Just False)
                  ]
              ]
      parseRuleBookYaml yaml `shouldBe` Right expected

    it "fails on invalid YAML" $ do
      parseRuleBookYaml (B8.pack "invalid: [[[") `shouldSatisfy` isLeft
