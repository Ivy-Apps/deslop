module Deslop.RuleBookSpec (spec) where

import Data.ByteString.Char8 qualified as B8
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Deslop.RuleBook (
    ForbiddenDto (..),
    RelativeModuleId (..),
    RuleBookDto (..),
    RuleDto (..),
    RuleId (..),
    parseRuleBookYaml,
 )
import Test.Hspec

spec :: Spec
spec = do
    describe "parseRuleBookYaml" $ do
        it "parses a forbidden import rule without a description" $ do
            let yaml =
                    B8.pack
                        "name: Demo Rulebook\n\
                        \rules:\n\
                        \  - id: no-ui\n\
                        \    target:\n\
                        \      - \"@/client/core/**/*\"\n\
                        \    forbidden:\n\
                        \      - import: react\n\
                        \      - import: \"@/client/components/**/*\"\n\
                        \        transitive: false\n"
            let expected =
                    RuleBookDto
                        { name = "Demo Rulebook"
                        , rules =
                            [ RuleDto
                                { id = RuleId "no-ui"
                                , description = Nothing
                                , target = RelativeModuleId "@/client/core/**/*" :| []
                                , forbidden =
                                    [ ForbiddenImportDto
                                        { target = RelativeModuleId "react"
                                        , transitive = Nothing
                                        }
                                    , ForbiddenImportDto
                                        { target = RelativeModuleId "@/client/components/**/*"
                                        , transitive = Just False
                                        }
                                    ]
                                }
                            ]
                        }
            parseRuleBookYaml yaml `shouldBe` Right expected

        it "parses a forbidden transitive import rule" $ do
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
                        { name = T.pack "Demo Rulebook"
                        , rules =
                            [ RuleDto
                                { id = RuleId "no-react-in-core"
                                , description = Just "The core must not have React dependencies"
                                , target =
                                    RelativeModuleId (T.pack "@/client/core/**/*")
                                        :| [ RelativeModuleId (T.pack "@/server/**/*")
                                           , RelativeModuleId (T.pack "@/shared/**/*")
                                           ]
                                , forbidden =
                                    [ ForbiddenImportDto
                                        { target = RelativeModuleId "react"
                                        , transitive = Just True
                                        }
                                    ]
                                }
                            ]
                        }
            parseRuleBookYaml yaml `shouldBe` Right expected

        it "fails on invalid YAML" $ do
            parseRuleBookYaml (B8.pack "invalid: [[[") `shouldSatisfy` isLeft
