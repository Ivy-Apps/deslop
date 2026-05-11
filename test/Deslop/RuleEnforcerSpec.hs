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
import TestUtils (mkExistsModuleDto, mkForbiddenImportDto, mkImportNode, mkUsesImportDto, requireRight)
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
                , description = "Rulebook used for testing."
                , rules =
                    [ RuleDto
                        { id = RuleId "no-forbids-import"
                        , description = "Forbids modules must not be imported."
                        , target = GlobDto "@/components/**"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbids = Just [mkForbiddenImportDto "@/forbids/**" False]
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
                , description = "Rulebook used for testing"
                , rules =
                    [ RuleDto
                        { id = RuleId "no-forbids-import"
                        , description = "Forbids modules must not be transitively imported."
                        , target = GlobDto "@/components/**"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbids = Just [mkForbiddenImportDto "@/forbids/**" True]
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
            RulebookDto
                { id = "domain-rules"
                , name = "Domain Rules"
                , description = "Domain rulebook."
                , rules =
                    [ RuleDto
                        { id = RuleId "no-react-in-domain"
                        , description = "Domain layer must not depend on React."
                        , target = GlobDto "@/domain/**"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbids = Just [mkForbiddenImportDto "react" True]
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
                , description = "Exists rulebook."
                , rules =
                    [ RuleDto
                        { id = RuleId "requires-spec"
                        , description = "Every ViewModel must have a spec file."
                        , target = GlobDto "@/features/**/use{{FileName}}ViewModel"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbids = Nothing
                        , uses = Nothing
                        , usesOptional = Nothing
                        , exists =
                            Just
                                [ mkExistsModuleDto "{{TARGET_DIR}}/use{{FileName}}ViewModel.spec"
                                ]
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

usesRulebook :: Rulebook
usesRulebook =
    fromRight (error "usesRulebook: invalid fixture") $
        ruleBookFromDto
            RulebookDto
                { id = "uses-rules"
                , name = "Uses Rules"
                , description = "Uses rulebook"
                , rules =
                    [ RuleDto
                        { id = RuleId "container-wires-state-event"
                        , description = "Containers must wire their StateEvent."
                        , target = GlobDto "@/features/**/{{FileName}}Container"
                        , exclude = Nothing
                        , executionContext = Nothing
                        , forbids = Nothing
                        , uses = Just [mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}StateEvent" False]
                        , usesOptional = Nothing
                        , exists = Nothing
                        , example = Nothing
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
                            RulebookDto
                                { id = "bad-rules"
                                , name = "Bad Rules"
                                , description = "bad rules"
                                , rules =
                                    [ RuleDto
                                        { id = RuleId "wildcard-exists"
                                        , description = "wildcard exists"
                                        , target = GlobDto "@/features/**/*"
                                        , exclude = Nothing
                                        , executionContext = Nothing
                                        , forbids = Nothing
                                        , uses = Nothing
                                        , usesOptional = Nothing
                                        , exists = Just [mkExistsModuleDto "{{TARGET_DIR}}/**/*.spec"]
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
                            RulebookDto
                                { id = "uses-rules"
                                , name = "Uses Rules"
                                , description = "uses rulebok"
                                , rules =
                                    [ RuleDto
                                        { id = RuleId "container-wires-all"
                                        , description = "Containers must wire all their dependencies."
                                        , target = GlobDto "@/features/**/{{FileName}}Container"
                                        , exclude = Nothing
                                        , executionContext = Nothing
                                        , forbids = Nothing
                                        , uses =
                                            Just
                                                [ mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}StateEvent" False
                                                , mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}View" False
                                                ]
                                        , usesOptional = Nothing
                                        , exists = Nothing
                                        , example = Nothing
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
                            RulebookDto
                                { id = "uses-rules"
                                , name = "Uses Rules"
                                , description = "uses rules"
                                , rules =
                                    [ RuleDto
                                        { id = RuleId "page-uses-container"
                                        , description = "page uses container"
                                        , target = GlobDto "@/app/**/page"
                                        , exclude = Nothing
                                        , executionContext = Nothing
                                        , forbids = Nothing
                                        , uses = Just [mkUsesImportDto "@/features/**/*Container" False]
                                        , usesOptional = Nothing
                                        , exists = Nothing
                                        , example = Nothing
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
                        RulebookDto
                            { id = "uses-rules"
                            , name = "Uses Rules"
                            , description = "uses rb"
                            , rules =
                                [ RuleDto
                                    { id = RuleId "container-wires-state-event-transitively"
                                    , description = "Containers must transitively wire their StateEvent."
                                    , target = GlobDto "@/features/**/{{FileName}}Container"
                                    , exclude = Nothing
                                    , executionContext = Nothing
                                    , forbids = Nothing
                                    , uses = Just [mkUsesImportDto "{{TARGET_DIR}}/{{FileName}}StateEvent" True]
                                    , usesOptional = Nothing
                                    , exists = Nothing
                                    , example = Nothing
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
                            RulebookDto
                                { id = "uses-rules"
                                , name = "Uses Rules"
                                , description = "uses"
                                , rules =
                                    [ RuleDto
                                        { id = RuleId "domain-must-use-logger"
                                        , description = "domain must use logger"
                                        , target = GlobDto "@/domain/**"
                                        , exclude = Nothing
                                        , executionContext = Nothing
                                        , forbids = Nothing
                                        , uses = Just [mkUsesImportDto "@/infrastructure/**/*Logger" True]
                                        , usesOptional = Nothing
                                        , exists = Nothing
                                        , example = Nothing
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
