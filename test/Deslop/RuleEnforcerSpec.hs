module Deslop.RuleEnforcerSpec (spec) where

import Deslop.AST (AstModule (..))
import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Problem (Problem (..))
import Deslop.RuleEnforcer (enforceRulebooks)
import Deslop.Rulebook (ForbiddenDto (..), GlobDto (..), RuleDto (..), RuleId (..), Rulebook, RulebookDto (..), RulebookId (..), ruleBookFromDto)
import Doubles.FileSystem (mockFiles, runMockRoFileSystem)
import Effectful (runEff)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (defaultTsConfig, mkImportNode)
import TypeScript.ModuleResolver (moduleIdUnsafe)

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
runTest m =
    runEff
        . runReportProblem
        . runReader (buildModuleGraph [])
        . runReader [testRulebook]
        . runReader defaultTsConfig
        . runMockRoFileSystem (mockFiles [])
        $ do
            enforceRulebooks m
            getProblems

runTransitiveTestWith :: [Rulebook] -> [AstModule] -> AstModule -> IO [Problem]
runTransitiveTestWith rulebooks allModules m =
    runEff
        . runReportProblem
        . runReader (buildModuleGraph allModules)
        . runReader rulebooks
        . runReader defaultTsConfig
        . runMockRoFileSystem (mockFiles [])
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
