module Deslop.RuleEnforcerSpec (spec) where

import Deslop.AST (AstModule (..), AstNode (..))
import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Problem (Problem (..))
import Deslop.RuleEnforcer (enforceRulebooks)
import Deslop.Rulebook (ForbiddenDto (..), GlobDto (..), RuleDto (..), RuleId (..), Rulebook, RulebookDto (..), RulebookId (..), ruleBookFromDto)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Test.Hspec (Spec, describe, it, shouldBe)
import TypeScript.ModuleResolver (moduleIdUnsafe)

testRulebook :: Rulebook
testRulebook =
    fromRight (error "testRulebook: invalid fixture") $
        ruleBookFromDto
            RulebookDto
                { id = "test-rulebook"
                , name = "Test Rulebook"
                , description = Nothing
                , rules =
                    [ RuleDto
                        { id = RuleId "no-forbidden-import"
                        , description = Nothing
                        , target = GlobDto "@/components/**"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbidden = Just [ForbiddenImportDto (GlobDto "@/forbidden/**") Nothing]
                        , uses = Nothing
                        , usesOptional = Nothing
                        , exists = Nothing
                        , example = Nothing
                        , fix = "Remove the import"
                        }
                    ]
                }

runTest :: AstModule -> IO [Problem]
runTest m =
    runEff
        . runReportProblem
        . runReader (buildModuleGraph [])
        . runReader [testRulebook]
        $ do
            enforceRulebooks m
            getProblems

spec :: Spec
spec = describe "Deslop.RuleEnforcer" $ do
    describe "forbidden imports" $ do
        it "no violations" $ do
            let m =
                    AstModule
                        { id = moduleIdUnsafe "@/components/Button"
                        , nodes = [ImportNode {target = moduleIdUnsafe "react"}]
                        }
            problems <- runTest m
            problems `shouldBe` []

        it "direct import violation" $ do
            let m =
                    AstModule
                        { id = moduleIdUnsafe "@/components/Button"
                        , nodes = [ImportNode {target = moduleIdUnsafe "@/forbidden/module"}]
                        }
            problems <- runTest m
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbidden-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Module '@/components/Button' directly imports '@/forbidden/module'."
                                , fix = "Remove the import"
                                }
                           ]
