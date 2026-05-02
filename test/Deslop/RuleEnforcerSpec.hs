module Deslop.RuleEnforcerSpec (spec) where

import Deslop.AST (AstModule (..))
import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Problem (Problem (..))
import Deslop.RuleEnforcer (enforceRulebooks)
import Deslop.Rulebook (ForbiddenDto (..), GlobDto (..), RuleDto (..), RuleId (..), Rulebook, RulebookDto (..), RulebookId (..), ruleBookFromDto)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import TestUtils (mkImportNode, requireRight)
import TypeScript.ModuleResolver (moduleIdUnsafe)
import Types (DeslopError (..))

mkModule :: Text -> [Text] -> AstModule
mkModule mid deps =
    AstModule
        { id = moduleIdUnsafe mid
        , nodes = map mkImportNode deps
        }

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

testTransitiveRulebook :: Rulebook
testTransitiveRulebook =
    fromRight (error "testTransitiveRulebook: invalid fixture") $
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
                        , forbidden = Just [ForbiddenImportDto (GlobDto "@/forbidden/**") (Just True)]
                        , uses = Nothing
                        , usesOptional = Nothing
                        , exists = Nothing
                        , example = Nothing
                        , fix = "Remove the import"
                        }
                    ]
                }

runTest :: AstModule -> IO [Problem]
runTest m = do
    problemsRes <- runEff
        . runErrorNoCallStack @DeslopError
        . runReportProblem
        . runReader (buildModuleGraph [])
        . runReader [testRulebook]
        $ do
            enforceRulebooks m
            getProblems
    ps <- requireRight show problemsRes
    pure ps

runTransitiveTestWith :: [Rulebook] -> [AstModule] -> AstModule -> IO [Problem]
runTransitiveTestWith rulebooks allModules m =
    fmap (either (error . show) id)
        . runEff
        . runErrorNoCallStack @DeslopError
        . runReportProblem
        . runReader (buildModuleGraph allModules)
        . runReader rulebooks
        $ do
            enforceRulebooks m
            getProblems

runTransitiveTest :: [AstModule] -> AstModule -> IO [Problem]
runTransitiveTest = runTransitiveTestWith [testTransitiveRulebook]

domainRulebook :: Rulebook
domainRulebook =
    fromRight (error "domainRulebook: invalid fixture") $
        ruleBookFromDto
            RulebookDto
                { id = "domain-rules"
                , name = "Domain Rules"
                , description = Nothing
                , rules =
                    [ RuleDto
                        { id = RuleId "no-react-in-domain"
                        , description = Nothing
                        , target = GlobDto "@/domain/**"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbidden = Just [ForbiddenImportDto (GlobDto "react") (Just True)]
                        , uses = Nothing
                        , usesOptional = Nothing
                        , exists = Nothing
                        , example = Nothing
                        , fix = "Move React dependencies out of the domain layer."
                        }
                    ]
                }

existsRulebook :: Rulebook
existsRulebook =
    fromRight (error "existsRulebook: invalid fixture") $
        ruleBookFromDto
            RulebookDto
                { id = "exists-rules"
                , name = "Exists Rules"
                , description = Nothing
                , rules =
                    [ RuleDto
                        { id = RuleId "requires-spec"
                        , description = Nothing
                        , target = GlobDto "@/features/**/use{{FileName}}ViewModel"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbidden = Nothing
                        , uses = Nothing
                        , usesOptional = Nothing
                        , exists = Just [GlobDto "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"]
                        , example = Nothing
                        , fix = "Create the spec file."
                        }
                    ]
                }

runExistsTest :: [Rulebook] -> [AstModule] -> AstModule -> IO (Either DeslopError [Problem])
runExistsTest rulebooks allModules m =
    runEff
        . runErrorNoCallStack @DeslopError
        . runReportProblem
        . runReader (buildModuleGraph allModules)
        . runReader rulebooks
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
                        , nodes = [mkImportNode "react"]
                        }
            problems <- runTest m
            problems `shouldBe` []

        it "direct import violation" $ do
            let m =
                    AstModule
                        { id = moduleIdUnsafe "@/components/Button"
                        , nodes = [mkImportNode "@/forbidden/module"]
                        }
            problems <- runTest m
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbidden-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Module '@/components/Button' directly imports '@/forbidden/module'.\n```ts\nimport { ... } from '@/forbidden/module'\n```"
                                , fix = "Remove the import"
                                }
                           ]

    describe "transitive import violations" $ do
        it "no violations when no forbidden module is reachable" $ do
            let button = mkModule "@/components/Button" ["@/lib/util"]
                util = mkModule "@/lib/util" []
            problems <- runTransitiveTest [button, util] button
            problems `shouldBe` []

        it "single-hop transitive violation" $ do
            let button = mkModule "@/components/Button" ["@/forbidden/store"]
                forbidden = mkModule "@/forbidden/store" []
            problems <- runTransitiveTest [button, forbidden] button
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbidden-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Module '@/components/Button' transitively imports '@/forbidden/store' via: @/components/Button → @/forbidden/store.\n```ts\nimport { ... } from '@/forbidden/store'\n```"
                                , fix = "Remove the import"
                                }
                           ]

        it "multi-hop transitive violation" $ do
            let button = mkModule "@/components/Button" ["@/lib/util"]
                util = mkModule "@/lib/util" ["@/forbidden/store"]
                forbidden = mkModule "@/forbidden/store" []
            problems <- runTransitiveTest [button, util, forbidden] button
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbidden-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Module '@/components/Button' transitively imports '@/forbidden/store' via: @/components/Button → @/lib/util → @/forbidden/store.\n```ts\nimport { ... } from '@/lib/util'\n```"
                                , fix = "Remove the import"
                                }
                           ]

        it "reports multiple reachable forbidden modules" $ do
            let button = mkModule "@/components/Button" ["@/lib/util", "@/lib/helpers"]
                util = mkModule "@/lib/util" ["@/forbidden/storeA"]
                helpers = mkModule "@/lib/helpers" ["@/forbidden/storeB"]
                forbiddenA = mkModule "@/forbidden/storeA" []
                forbiddenB = mkModule "@/forbidden/storeB" []
            problems <- runTransitiveTest [button, util, helpers, forbiddenA, forbiddenB] button
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbidden-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Module '@/components/Button' transitively imports '@/forbidden/storeA' via: @/components/Button → @/lib/util → @/forbidden/storeA.\n```ts\nimport { ... } from '@/lib/util'\n```"
                                , fix = "Remove the import"
                                }
                           , RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbidden-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Module '@/components/Button' transitively imports '@/forbidden/storeB' via: @/components/Button → @/lib/helpers → @/forbidden/storeB.\n```ts\nimport { ... } from '@/lib/helpers'\n```"
                                , fix = "Remove the import"
                                }
                           ]

        it "domain module must not transitively import react" $ do
            let useCase = mkModule "@/domain/LoginUseCase" ["@/domain/UserRepository"]
                repo = mkModule "@/domain/UserRepository" ["@/infrastructure/HttpClient"]
                http = mkModule "@/infrastructure/HttpClient" ["react"]
                react = mkModule "react" []
            problems <- runTransitiveTestWith [domainRulebook] [useCase, repo, http, react] useCase
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "domain-rules"
                                , rule = RuleId "no-react-in-domain"
                                , badModule = moduleIdUnsafe "@/domain/LoginUseCase"
                                , description = "Module '@/domain/LoginUseCase' transitively imports 'react' via: @/domain/LoginUseCase → @/domain/UserRepository → @/infrastructure/HttpClient → react.\n```ts\nimport { ... } from '@/domain/UserRepository'\n```"
                                , fix = "Move React dependencies out of the domain layer."
                                }
                           ]

    describe "exists enforcement" $ do
        it "no violation when the required module exists in the graph" $ do
            let viewModel = mkModule "@/features/home/useHomeViewModel" []
                vmSpec = mkModule "@/features/home/useHomeViewModel.spec" []
            result <- runExistsTest [existsRulebook] [viewModel, vmSpec] viewModel
            result `shouldBe` Right []

        it "reports a violation when the required module is missing from the graph" $ do
            let viewModel = mkModule "@/features/home/useHomeViewModel" []
            result <- runExistsTest [existsRulebook] [viewModel] viewModel
            result
                `shouldBe` Right
                    [ RuleViolation
                        { rulebook = RulebookId "exists-rules"
                        , rule = RuleId "requires-spec"
                        , badModule = moduleIdUnsafe "@/features/home/useHomeViewModel"
                        , description = "Module '@/features/home/useHomeViewModel' requires '@/features/home/useHomeViewModel.spec' to exist."
                        , fix = "Create the spec file."
                        }
                    ]

        it "does not report a violation for a module that does not match the target" $ do
            let notAViewModel = mkModule "@/features/home/HomeView" []
            result <- runExistsTest [existsRulebook] [notAViewModel] notAViewModel
            result `shouldBe` Right []

        it "throws InvalidRuleConfig when an exists pattern contains wildcards" $ do
            let wildcardRulebook =
                    fromRight (error "wildcardRulebook: invalid fixture") $
                        ruleBookFromDto
                            RulebookDto
                                { id = "bad-rules"
                                , name = "Bad Rules"
                                , description = Nothing
                                , rules =
                                    [ RuleDto
                                        { id = RuleId "wildcard-exists"
                                        , description = Nothing
                                        , target = GlobDto "@/features/**/*"
                                        , exclude = Nothing
                                        , executionContext = Nothing
                                        , forbidden = Nothing
                                        , uses = Nothing
                                        , usesOptional = Nothing
                                        , exists = Just [GlobDto "{{TARGET_DIR}}/**/*.spec"]
                                        , example = Nothing
                                        , fix = "Fix it."
                                        }
                                    ]
                                }
                m = mkModule "@/features/home/HomeView" []
            result <- runExistsTest [wildcardRulebook] [m] m
            case result of
                Left (InvalidRuleConfig msg) ->
                    msg `shouldSatisfy` T.isInfixOf "wildcard-exists"
                other ->
                    expectationFailure $ "expected InvalidRuleConfig, got: " <> show other
