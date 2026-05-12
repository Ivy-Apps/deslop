module Deslop.RuleEnforcerSpec (spec) where

import Data.Text qualified as T
import Deslop.AST (AstModule (..))
import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Problem (Problem (..))
import Deslop.RuleEnforcer (enforceRulebooks)
import Deslop.Rulebook (GlobDto (..), RuleDto (..), RuleId (..), Rulebook, RulebookDto (..), RulebookId (..), ruleBookFromDto)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import TestUtils (mkAllowsImportDto, mkExistsModuleDto, mkForbidsImportDto, mkImportNode, mkUsesImportDto, requireRight, ruleDto, rulebookDto)
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
            rulebookDto
                { rules =
                    [ ruleDto
                        { id = RuleId "no-forbids-import"
                        , description = "Forbids modules must not be imported."
                        , target = GlobDto "@/components/**"
                        , forbids = Just [mkForbidsImportDto "@/forbids/**" False]
                        , fix = "Remove the import"
                        }
                    ]
                }

testTransitiveRulebook :: Rulebook
testTransitiveRulebook =
    fromRight (error "testTransitiveRulebook: invalid fixture") $
        ruleBookFromDto
            rulebookDto
                { rules =
                    [ ruleDto
                        { id = RuleId "no-forbids-import"
                        , description = "Forbids modules must not be transitively imported."
                        , target = GlobDto "@/components/**"
                        , forbids = Just [mkForbidsImportDto "@/forbids/**" True]
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
    requireRight show problemsRes

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
            rulebookDto
                { id = "domain-rules"
                , name = "Domain Rules"
                , description = "Domain rulebook."
                , rules =
                    [ ruleDto
                        { id = RuleId "no-react-in-domain"
                        , description = "Domain layer must not depend on React."
                        , target = GlobDto "@/domain/**"
                        , forbids = Just [mkForbidsImportDto "react" True]
                        , fix = "Move React dependencies out of the domain layer."
                        }
                    ]
                }

existsRulebook :: Rulebook
existsRulebook =
    fromRight (error "existsRulebook: invalid fixture") $
        ruleBookFromDto
            rulebookDto
                { id = "exists-rules"
                , name = "Exists Rules"
                , description = "Exists rulebook."
                , rules =
                    [ ruleDto
                        { id = RuleId "requires-spec"
                        , description = "Every ViewModel must have a spec file."
                        , target = GlobDto "@/features/**/use{{FileName}}ViewModel"
                        , exists = Just [mkExistsModuleDto "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"]
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

usesRulebook :: Rulebook
usesRulebook =
    fromRight (error "usesRulebook: invalid fixture") $
        ruleBookFromDto
            rulebookDto
                { id = "uses-rules"
                , name = "Uses Rules"
                , description = "Uses rulebook"
                , rules =
                    [ ruleDto
                        { id = RuleId "container-wires-state-event"
                        , description = "Containers must wire their StateEvent."
                        , target = GlobDto "@/features/**/{{FileName}}Container"
                        , uses = Just [mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}StateEvent" False]
                        , fix = "Import the StateEvent."
                        }
                    ]
                }

runUsesTest :: [AstModule] -> AstModule -> IO (Either DeslopError [Problem])
runUsesTest allModules m =
    runEff
        . runErrorNoCallStack @DeslopError
        . runReportProblem
        . runReader (buildModuleGraph allModules)
        . runReader [usesRulebook]
        $ do
            enforceRulebooks m
            getProblems

spec :: Spec
spec = describe "Deslop.RuleEnforcer" $ do
    describe "forbids imports" $ do
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
                        , nodes = [mkImportNode "@/forbids/module"]
                        }
            problems <- runTest m
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbids-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Forbids modules must not be imported.\n\nModule '@/components/Button' directly imports '@/forbids/module'.\n```ts\nimport { ... } from '@/forbids/module'\n```"
                                , fix = "Remove the import"
                                }
                           ]

    describe "transitive import violations" $ do
        it "no violations when no forbids module is reachable" $ do
            let button = mkModule "@/components/Button" ["@/lib/util"]
                util = mkModule "@/lib/util" []
            problems <- runTransitiveTest [button, util] button
            problems `shouldBe` []

        it "single-hop transitive violation" $ do
            let button = mkModule "@/components/Button" ["@/forbids/store"]
                forbids = mkModule "@/forbids/store" []
            problems <- runTransitiveTest [button, forbids] button
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbids-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Forbids modules must not be transitively imported.\n\nModule '@/components/Button' transitively imports '@/forbids/store' via: @/components/Button → @/forbids/store.\n```ts\nimport { ... } from '@/forbids/store'\n```"
                                , fix = "Remove the import"
                                }
                           ]

        it "multi-hop transitive violation" $ do
            let button = mkModule "@/components/Button" ["@/lib/util"]
                util = mkModule "@/lib/util" ["@/forbids/store"]
                forbids = mkModule "@/forbids/store" []
            problems <- runTransitiveTest [button, util, forbids] button
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbids-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Forbids modules must not be transitively imported.\n\nModule '@/components/Button' transitively imports '@/forbids/store' via: @/components/Button → @/lib/util → @/forbids/store.\n```ts\nimport { ... } from '@/lib/util'\n```"
                                , fix = "Remove the import"
                                }
                           ]

        it "reports multiple reachable forbids modules" $ do
            let button = mkModule "@/components/Button" ["@/lib/util", "@/lib/helpers"]
                util = mkModule "@/lib/util" ["@/forbids/storeA"]
                helpers = mkModule "@/lib/helpers" ["@/forbids/storeB"]
                forbidsA = mkModule "@/forbids/storeA" []
                forbidsB = mkModule "@/forbids/storeB" []
            problems <- runTransitiveTest [button, util, helpers, forbidsA, forbidsB] button
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbids-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Forbids modules must not be transitively imported.\n\nModule '@/components/Button' transitively imports '@/forbids/storeA' via: @/components/Button → @/lib/util → @/forbids/storeA.\n```ts\nimport { ... } from '@/lib/util'\n```"
                                , fix = "Remove the import"
                                }
                           , RuleViolation
                                { rulebook = RulebookId "test-rulebook"
                                , rule = RuleId "no-forbids-import"
                                , badModule = moduleIdUnsafe "@/components/Button"
                                , description = "Forbids modules must not be transitively imported.\n\nModule '@/components/Button' transitively imports '@/forbids/storeB' via: @/components/Button → @/lib/helpers → @/forbids/storeB.\n```ts\nimport { ... } from '@/lib/helpers'\n```"
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
                                , description = "Domain layer must not depend on React.\n\nModule '@/domain/LoginUseCase' transitively imports 'react' via: @/domain/LoginUseCase → @/domain/UserRepository → @/infrastructure/HttpClient → react.\n```ts\nimport { ... } from '@/domain/UserRepository'\n```"
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
                        , description = "Every ViewModel must have a spec file.\n\nModule '@/features/home/useHomeViewModel' requires '@/features/home/useHomeViewModel.spec' to exist."
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
                            rulebookDto
                                { id = "bad-rules"
                                , name = "Bad Rules"
                                , description = "bad rules"
                                , rules =
                                    [ ruleDto
                                        { id = RuleId "wildcard-exists"
                                        , description = "wildcard exists"
                                        , target = GlobDto "@/features/**/*"
                                        , exists = Just [mkExistsModuleDto "{{TARGET_DIR}}/**/*.spec"]
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

    describe "uses enforcement" $ do
        it "no violation when module imports a matching module" $ do
            let container = mkModule "@/features/home/HomeContainer" ["@/features/home/HomeStateEvent"]
            result <- runUsesTest [container] container
            result `shouldBe` Right []

        it "reports a violation when no import matches the uses pattern" $ do
            let container = mkModule "@/features/home/HomeContainer" ["@/features/home/HomeView"]
            result <- runUsesTest [container] container
            result
                `shouldBe` Right
                    [ RuleViolation
                        { rulebook = RulebookId "uses-rules"
                        , rule = RuleId "container-wires-state-event"
                        , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                        , description = "Containers must wire their StateEvent.\n\nModule '@/features/home/HomeContainer' must import '@/features/home/HomeStateEvent'."
                        , fix = "Import the StateEvent."
                        }
                    ]

        it "reports a violation when module has no imports at all" $ do
            let container = mkModule "@/features/home/HomeContainer" []
            result <- runUsesTest [container] container
            result
                `shouldBe` Right
                    [ RuleViolation
                        { rulebook = RulebookId "uses-rules"
                        , rule = RuleId "container-wires-state-event"
                        , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                        , description = "Containers must wire their StateEvent.\n\nModule '@/features/home/HomeContainer' must import '@/features/home/HomeStateEvent'."
                        , fix = "Import the StateEvent."
                        }
                    ]

        it "no violation for a module that does not match the target" $ do
            let notAContainer = mkModule "@/features/home/HomeView" []
            result <- runUsesTest [notAContainer] notAContainer
            result `shouldBe` Right []

        it "reports one violation per unmatched uses pattern" $ do
            let multiUsesRulebook =
                    fromRight (error "multiUsesRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { id = "uses-rules"
                                , name = "Uses Rules"
                                , description = "uses rulebok"
                                , rules =
                                    [ ruleDto
                                        { id = RuleId "container-wires-all"
                                        , description = "Containers must wire all their dependencies."
                                        , target = GlobDto "@/features/**/{{FileName}}Container"
                                        , uses =
                                            Just
                                                [ mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}StateEvent" False
                                                , mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}View" False
                                                ]
                                        , fix = "Wire the Container."
                                        }
                                    ]
                                }
                container = mkModule "@/features/home/HomeContainer" []
            result <- runExistsTest [multiUsesRulebook] [container] container
            result
                `shouldBe` Right
                    [ RuleViolation
                        { rulebook = RulebookId "uses-rules"
                        , rule = RuleId "container-wires-all"
                        , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                        , description = "Containers must wire all their dependencies.\n\nModule '@/features/home/HomeContainer' must import '@/features/home/HomeStateEvent'."
                        , fix = "Wire the Container."
                        }
                    , RuleViolation
                        { rulebook = RulebookId "uses-rules"
                        , rule = RuleId "container-wires-all"
                        , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                        , description = "Containers must wire all their dependencies.\n\nModule '@/features/home/HomeContainer' must import '@/features/home/HomeView'."
                        , fix = "Wire the Container."
                        }
                    ]

        it "wildcard uses pattern matches any qualifying import" $ do
            let wildcardUsesRulebook =
                    fromRight (error "wildcardUsesRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { id = "uses-rules"
                                , name = "Uses Rules"
                                , description = "uses rules"
                                , rules =
                                    [ ruleDto
                                        { id = RuleId "page-uses-container"
                                        , description = "page uses container"
                                        , target = GlobDto "@/app/**/page"
                                        , uses = Just [mkUsesImportDto "@/features/**/*Container" False]
                                        , fix = "Import a Container."
                                        }
                                    ]
                                }
                page = mkModule "@/app/home/page" ["@/features/home/HomeContainer"]
            result <- runExistsTest [wildcardUsesRulebook] [page] page
            result `shouldBe` Right []

    describe "transitive uses enforcement" $ do
        let usesTransitiveRulebook =
                fromRight (error "usesTransitiveRulebook: invalid fixture") $
                    ruleBookFromDto
                        rulebookDto
                            { id = "uses-rules"
                            , name = "Uses Rules"
                            , description = "uses rb"
                            , rules =
                                [ ruleDto
                                    { id = RuleId "container-wires-state-event-transitively"
                                    , description = "Containers must transitively wire their StateEvent."
                                    , target = GlobDto "@/features/**/{{FileName}}Container"
                                    , uses = Just [mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}StateEvent" True]
                                    , fix = "Import the StateEvent."
                                    }
                                ]
                            }
            runUsesTransitiveTest allModules m =
                fmap (either (error . show) id)
                    . runEff
                    . runErrorNoCallStack @DeslopError
                    . runReportProblem
                    . runReader (buildModuleGraph allModules)
                    . runReader [usesTransitiveRulebook]
                    $ do
                        enforceRulebooks m
                        getProblems

        it "no violation when module directly imports the required module" $ do
            let container = mkModule "@/features/home/HomeContainer" ["@/features/home/HomeStateEvent"]
            problems <- runUsesTransitiveTest [container] container
            problems `shouldBe` []

        it "no violation when module transitively imports the required module via an intermediary" $ do
            let container = mkModule "@/features/home/HomeContainer" ["@/features/home/HomeView"]
                view = mkModule "@/features/home/HomeView" ["@/features/home/HomeStateEvent"]
                stateEvent = mkModule "@/features/home/HomeStateEvent" []
            problems <- runUsesTransitiveTest [container, view, stateEvent] container
            problems `shouldBe` []

        it "reports a violation when the required module is not reachable" $ do
            let container = mkModule "@/features/home/HomeContainer" ["@/features/home/HomeView"]
                view = mkModule "@/features/home/HomeView" []
            problems <- runUsesTransitiveTest [container, view] container
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "uses-rules"
                                , rule = RuleId "container-wires-state-event-transitively"
                                , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                                , description = "Containers must transitively wire their StateEvent.\n\nModule '@/features/home/HomeContainer' must transitively import '@/features/home/HomeStateEvent'."
                                , fix = "Import the StateEvent."
                                }
                           ]

        it "reports a violation when module has no imports at all" $ do
            let container = mkModule "@/features/home/HomeContainer" []
            problems <- runUsesTransitiveTest [container] container
            problems
                `shouldBe` [ RuleViolation
                                { rulebook = RulebookId "uses-rules"
                                , rule = RuleId "container-wires-state-event-transitively"
                                , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                                , description = "Containers must transitively wire their StateEvent.\n\nModule '@/features/home/HomeContainer' must transitively import '@/features/home/HomeStateEvent'."
                                , fix = "Import the StateEvent."
                                }
                           ]

        it "no violation for a module that does not match the target" $ do
            let notAContainer = mkModule "@/features/home/HomeView" []
            problems <- runUsesTransitiveTest [notAContainer] notAContainer
            problems `shouldBe` []

        it "wildcard transitive uses pattern matches a reachable module" $ do
            let wildcardTransitiveRulebook =
                    fromRight (error "wildcardTransitiveRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { id = "uses-rules"
                                , name = "Uses Rules"
                                , description = "uses"
                                , rules =
                                    [ ruleDto
                                        { id = RuleId "domain-must-use-logger"
                                        , description = "domain must use logger"
                                        , target = GlobDto "@/domain/**"
                                        , uses = Just [mkUsesImportDto "@/infrastructure/**/*Logger" True]
                                        , fix = "Ensure a logger is used."
                                        }
                                    ]
                                }
                runWildcardTest allModules m =
                    fmap (either (error . show) id)
                        . runEff
                        . runErrorNoCallStack @DeslopError
                        . runReportProblem
                        . runReader (buildModuleGraph allModules)
                        . runReader [wildcardTransitiveRulebook]
                        $ do
                            enforceRulebooks m
                            getProblems
                useCase = mkModule "@/domain/LoginUseCase" ["@/domain/LoginService"]
                service = mkModule "@/domain/LoginService" ["@/infrastructure/http/HttpLogger"]
                logger = mkModule "@/infrastructure/http/HttpLogger" []
            problems <- runWildcardTest [useCase, service, logger] useCase
            problems `shouldBe` []

    describe "allows override" $ do
        describe "direct forbids with allows" $ do
            let directAllowsRulebook =
                    fromRight (error "directAllowsRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "no-forbidden-imports"
                                        , description = "Forbidden imports not allowed."
                                        , target = GlobDto "@/components/**"
                                        , forbids = Just [mkForbidsImportDto "@/forbids/**" False]
                                        , allows = Just [mkAllowsImportDto "@/allowed/**"]
                                        , fix = "Remove the import."
                                        }
                                    ]
                                }
                sharedOnlyRulebook =
                    fromRight (error "sharedOnlyRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "shared-only"
                                        , description = "Only shared imports allowed."
                                        , target = GlobDto "@/components/**"
                                        , forbids = Just [mkForbidsImportDto "**" False]
                                        , allows = Just [mkAllowsImportDto "@/shared/**"]
                                        , fix = "Use shared modules only."
                                        }
                                    ]
                                }
                domainPurityRulebook =
                    fromRight (error "domainPurityRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "domain-purity"
                                        , description = "Domain modules may only import from domain or shared layers."
                                        , target = GlobDto "@/domain/**"
                                        , forbids = Just [mkForbidsImportDto "**" False]
                                        , allows = Just (map mkAllowsImportDto ["@/domain/**", "@/shared/**"])
                                        , fix = "Move the dependency to the correct layer."
                                        }
                                    ]
                                }

            it "no violation when import matches allows pattern" $ do
                let m = mkModule "@/components/Button" ["@/allowed/utils"]
                problems <- runTransitiveTestWith [directAllowsRulebook] [m] m
                problems `shouldBe` []

            it "violation when import matches forbids but not allows" $ do
                let m = mkModule "@/components/Button" ["@/forbids/store"]
                problems <- runTransitiveTestWith [directAllowsRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "no-forbidden-imports"
                                    , badModule = moduleIdUnsafe "@/components/Button"
                                    , description = "Forbidden imports not allowed.\n\nModule '@/components/Button' directly imports '@/forbids/store'.\n```ts\nimport { ... } from '@/forbids/store'\n```"
                                    , fix = "Remove the import."
                                    }
                               ]

            it "no violation when forbids all (**) but import matches allows (shared module exception)" $ do
                let m = mkModule "@/components/Button" ["@/shared/utils"]
                problems <- runTransitiveTestWith [sharedOnlyRulebook] [m] m
                problems `shouldBe` []

            it "violation when forbids all (**) and import does not match any allows pattern" $ do
                let m = mkModule "@/components/Button" ["react"]
                problems <- runTransitiveTestWith [sharedOnlyRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "shared-only"
                                    , badModule = moduleIdUnsafe "@/components/Button"
                                    , description = "Only shared imports allowed.\n\nModule '@/components/Button' directly imports 'react'.\n```ts\nimport { ... } from 'react'\n```"
                                    , fix = "Use shared modules only."
                                    }
                               ]

            it "no violation: domain purity - same-domain import allowed" $ do
                let m = mkModule "@/domain/LoginUseCase" ["@/domain/UserRepository"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems `shouldBe` []

            it "no violation: domain purity - shared layer import allowed" $ do
                let m = mkModule "@/domain/LoginUseCase" ["@/shared/Result"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems `shouldBe` []

            it "no violation: domain purity - deeply nested module imports another domain sub-namespace" $ do
                let m = mkModule "@/domain/user/login/LoginUseCase" ["@/domain/user/profile/ProfileRepository"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems `shouldBe` []

            it "violation: domain purity - infrastructure import blocked by forbids all" $ do
                let m = mkModule "@/domain/LoginUseCase" ["@/infrastructure/HttpClient"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "domain-purity"
                                    , badModule = moduleIdUnsafe "@/domain/LoginUseCase"
                                    , description = "Domain modules may only import from domain or shared layers.\n\nModule '@/domain/LoginUseCase' directly imports '@/infrastructure/HttpClient'.\n```ts\nimport { ... } from '@/infrastructure/HttpClient'\n```"
                                    , fix = "Move the dependency to the correct layer."
                                    }
                               ]

            it "violation: domain purity - external library import blocked" $ do
                let m = mkModule "@/domain/LoginUseCase" ["react"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "domain-purity"
                                    , badModule = moduleIdUnsafe "@/domain/LoginUseCase"
                                    , description = "Domain modules may only import from domain or shared layers.\n\nModule '@/domain/LoginUseCase' directly imports 'react'.\n```ts\nimport { ... } from 'react'\n```"
                                    , fix = "Move the dependency to the correct layer."
                                    }
                               ]

            it "violation: domain purity - deeply nested domain module imports infrastructure layer" $ do
                let m = mkModule "@/domain/user/login/LoginUseCase" ["@/infrastructure/db/UserDbRepository"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "domain-purity"
                                    , badModule = moduleIdUnsafe "@/domain/user/login/LoginUseCase"
                                    , description = "Domain modules may only import from domain or shared layers.\n\nModule '@/domain/user/login/LoginUseCase' directly imports '@/infrastructure/db/UserDbRepository'.\n```ts\nimport { ... } from '@/infrastructure/db/UserDbRepository'\n```"
                                    , fix = "Move the dependency to the correct layer."
                                    }
                               ]

        describe "transitive forbids with allows" $ do
            let storeAllowsRulebook =
                    fromRight (error "storeAllowsRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "no-transitive-store"
                                        , description = "Components must not transitively import store modules."
                                        , target = GlobDto "@/components/**"
                                        , forbids = Just [mkForbidsImportDto "@/store/**" True]
                                        , allows = Just [mkAllowsImportDto "@/store/ui-store"]
                                        , fix = "Remove the transitive store import."
                                        }
                                    ]
                                }
                domainPurityTransitiveRulebook =
                    fromRight (error "domainPurityTransitiveRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "domain-purity-transitive"
                                        , description = "Domain must not transitively reach non-domain/shared modules."
                                        , target = GlobDto "@/domain/**"
                                        , forbids = Just [mkForbidsImportDto "**" True]
                                        , allows = Just (map mkAllowsImportDto ["@/domain/**", "@/shared/**"])
                                        , fix = "Keep domain pure."
                                        }
                                    ]
                                }

            it "no violation when transitive import matches allows pattern" $ do
                let button = mkModule "@/components/Button" ["@/lib/hooks"]
                    hooks = mkModule "@/lib/hooks" ["@/store/ui-store"]
                    uiStore = mkModule "@/store/ui-store" []
                problems <- runTransitiveTestWith [storeAllowsRulebook] [button, hooks, uiStore] button
                problems `shouldBe` []

            it "violation when transitive import matches forbids but not allows" $ do
                let button = mkModule "@/components/Button" ["@/lib/hooks"]
                    hooks = mkModule "@/lib/hooks" ["@/store/app-store"]
                    appStore = mkModule "@/store/app-store" []
                problems <- runTransitiveTestWith [storeAllowsRulebook] [button, hooks, appStore] button
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "no-transitive-store"
                                    , badModule = moduleIdUnsafe "@/components/Button"
                                    , description = "Components must not transitively import store modules.\n\nModule '@/components/Button' transitively imports '@/store/app-store' via: @/components/Button → @/lib/hooks → @/store/app-store.\n```ts\nimport { ... } from '@/lib/hooks'\n```"
                                    , fix = "Remove the transitive store import."
                                    }
                               ]

            it "no violation: domain purity transitive - multi-hop chain through domain and shared only" $ do
                let useCase = mkModule "@/domain/LoginUseCase" ["@/domain/UserRepository"]
                    repo = mkModule "@/domain/UserRepository" ["@/shared/Result"]
                    result = mkModule "@/shared/Result" []
                problems <- runTransitiveTestWith [domainPurityTransitiveRulebook] [useCase, repo, result] useCase
                problems `shouldBe` []

            it "violation: domain purity transitive - infra reachable via intermediate domain service" $ do
                let useCase = mkModule "@/domain/LoginUseCase" ["@/domain/AuthService"]
                    service = mkModule "@/domain/AuthService" ["@/infrastructure/HttpClient"]
                    http = mkModule "@/infrastructure/HttpClient" []
                problems <- runTransitiveTestWith [domainPurityTransitiveRulebook] [useCase, service, http] useCase
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "domain-purity-transitive"
                                    , badModule = moduleIdUnsafe "@/domain/LoginUseCase"
                                    , description = "Domain must not transitively reach non-domain/shared modules.\n\nModule '@/domain/LoginUseCase' transitively imports '@/infrastructure/HttpClient' via: @/domain/LoginUseCase → @/domain/AuthService → @/infrastructure/HttpClient.\n```ts\nimport { ... } from '@/domain/AuthService'\n```"
                                    , fix = "Keep domain pure."
                                    }
                               ]

        describe "cross-feature isolation" $ do
            -- {{TARGET_DIR}} expands to the directory of the matched module.
            -- e.g. @/features/home/HomeContainer → {{TARGET_DIR}} = @/features/home
            --      @/features/home/data/HomeRepository → {{TARGET_DIR}} = @/features/home/data
            -- This enforces directory-level isolation: a module may only import from its
            -- own directory (and any subdirectories, since ** spans segments).
            let dirIsolationRulebook =
                    fromRight (error "dirIsolationRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "dir-isolation"
                                        , description = "Feature modules may only import from their own directory."
                                        , target = GlobDto "@/features/**"
                                        , forbids = Just [mkForbidsImportDto "**" False]
                                        , allows = Just [mkAllowsImportDto "{{TARGET_DIR}}/**"]
                                        , fix = "Keep imports within the same directory or extract to shared."
                                        }
                                    ]
                                }
                -- Relaxed variant: also permits @/shared/** imports
                dirIsolationWithSharedRulebook =
                    fromRight (error "dirIsolationWithSharedRulebook: invalid fixture") $
                        ruleBookFromDto
                            rulebookDto
                                { rules =
                                    [ ruleDto
                                        { id = RuleId "dir-isolation-shared"
                                        , description = "Feature modules may only import from their own directory or shared."
                                        , target = GlobDto "@/features/**"
                                        , forbids = Just [mkForbidsImportDto "**" False]
                                        , allows = Just (map mkAllowsImportDto ["{{TARGET_DIR}}/**", "@/shared/**"])
                                        , fix = "Keep imports within the same directory or use shared modules."
                                        }
                                    ]
                                }

            it "no violation: top-level feature module imports sibling in same directory" $ do
                let m = mkModule "@/features/home/HomeContainer" ["@/features/home/HomeService"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems `shouldBe` []

            it "no violation: top-level feature module imports from its own subdirectory" $ do
                -- {{TARGET_DIR}} of @/features/home/HomeContainer = @/features/home
                -- @/features/home/** matches @/features/home/data/HomeRepository ✓
                let m = mkModule "@/features/home/HomeContainer" ["@/features/home/data/HomeRepository"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems `shouldBe` []

            it "violation: top-level feature module imports from a different feature" $ do
                let m = mkModule "@/features/home/HomeContainer" ["@/features/auth/AuthService"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation"
                                    , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                                    , description = "Feature modules may only import from their own directory.\n\nModule '@/features/home/HomeContainer' directly imports '@/features/auth/AuthService'.\n```ts\nimport { ... } from '@/features/auth/AuthService'\n```"
                                    , fix = "Keep imports within the same directory or extract to shared."
                                    }
                               ]

            it "violation: top-level feature module imports external library" $ do
                let m = mkModule "@/features/home/HomeContainer" ["react"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation"
                                    , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                                    , description = "Feature modules may only import from their own directory.\n\nModule '@/features/home/HomeContainer' directly imports 'react'.\n```ts\nimport { ... } from 'react'\n```"
                                    , fix = "Keep imports within the same directory or extract to shared."
                                    }
                               ]

            it "no violation: nested module imports from same nested directory" $ do
                -- {{TARGET_DIR}} of @/features/home/data/HomeRepository = @/features/home/data
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/home/data/HomeDataSource"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems `shouldBe` []

            it "violation (edge case): nested module cannot import from its parent directory" $ do
                -- {{TARGET_DIR}} = @/features/home/data — does not match @/features/home/HomeService
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/home/HomeService"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation"
                                    , badModule = moduleIdUnsafe "@/features/home/data/HomeRepository"
                                    , description = "Feature modules may only import from their own directory.\n\nModule '@/features/home/data/HomeRepository' directly imports '@/features/home/HomeService'.\n```ts\nimport { ... } from '@/features/home/HomeService'\n```"
                                    , fix = "Keep imports within the same directory or extract to shared."
                                    }
                               ]

            it "violation (edge case): nested module cannot import from sibling directory within same feature" $ do
                -- @/features/home/data/** does not match @/features/home/ui/HomeButton
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/home/ui/HomeButton"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation"
                                    , badModule = moduleIdUnsafe "@/features/home/data/HomeRepository"
                                    , description = "Feature modules may only import from their own directory.\n\nModule '@/features/home/data/HomeRepository' directly imports '@/features/home/ui/HomeButton'.\n```ts\nimport { ... } from '@/features/home/ui/HomeButton'\n```"
                                    , fix = "Keep imports within the same directory or extract to shared."
                                    }
                               ]

            it "violation: nested module imports from a completely different feature" $ do
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/auth/data/AuthRepository"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation"
                                    , badModule = moduleIdUnsafe "@/features/home/data/HomeRepository"
                                    , description = "Feature modules may only import from their own directory.\n\nModule '@/features/home/data/HomeRepository' directly imports '@/features/auth/data/AuthRepository'.\n```ts\nimport { ... } from '@/features/auth/data/AuthRepository'\n```"
                                    , fix = "Keep imports within the same directory or extract to shared."
                                    }
                               ]

            it "no violation: relaxed rule - top-level module imports from shared" $ do
                let m = mkModule "@/features/home/HomeContainer" ["@/shared/utils"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems `shouldBe` []

            it "no violation: relaxed rule - nested module imports from shared" $ do
                let m = mkModule "@/features/home/data/HomeRepository" ["@/shared/Result"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems `shouldBe` []

            it "violation: relaxed rule - cross-feature still blocked even though shared is allowed" $ do
                let m = mkModule "@/features/home/HomeContainer" ["@/features/auth/AuthService"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation-shared"
                                    , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                                    , description = "Feature modules may only import from their own directory or shared.\n\nModule '@/features/home/HomeContainer' directly imports '@/features/auth/AuthService'.\n```ts\nimport { ... } from '@/features/auth/AuthService'\n```"
                                    , fix = "Keep imports within the same directory or use shared modules."
                                    }
                               ]

            it "violation: relaxed rule - external library still blocked" $ do
                let m = mkModule "@/features/home/HomeContainer" ["react"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation-shared"
                                    , badModule = moduleIdUnsafe "@/features/home/HomeContainer"
                                    , description = "Feature modules may only import from their own directory or shared.\n\nModule '@/features/home/HomeContainer' directly imports 'react'.\n```ts\nimport { ... } from 'react'\n```"
                                    , fix = "Keep imports within the same directory or use shared modules."
                                    }
                               ]

            it "violation: relaxed rule - nested module's parent-dir import still blocked" $ do
                -- Even with shared allowed, the asymmetric edge case still holds:
                -- nested module cannot reach its own parent directory
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/home/HomeService"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems
                    `shouldBe` [ RuleViolation
                                    { rulebook = RulebookId "test-rulebook"
                                    , rule = RuleId "dir-isolation-shared"
                                    , badModule = moduleIdUnsafe "@/features/home/data/HomeRepository"
                                    , description = "Feature modules may only import from their own directory or shared.\n\nModule '@/features/home/data/HomeRepository' directly imports '@/features/home/HomeService'.\n```ts\nimport { ... } from '@/features/home/HomeService'\n```"
                                    , fix = "Keep imports within the same directory or use shared modules."
                                    }
                               ]
