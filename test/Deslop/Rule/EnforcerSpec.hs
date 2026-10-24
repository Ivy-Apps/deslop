module Deslop.Rule.EnforcerSpec (spec) where

import Deslop.CodeGraph (buildModuleGraph)
import Deslop.Error (DeslopError (..))
import Deslop.Module (Module (..), moduleNameUnsafe)
import Deslop.Problem (Problem (..), ViolationKind (..))
import Deslop.Rule.Book (RuleId (..), Rulebook, RulebookId (..))
import Deslop.Rule.Book.Compiler (CompileError (..), Field (..))
import Deslop.Rule.Book.Loader (RulebookLoadError (..))
import Deslop.Rule.Enforcer (enforceRulebooks)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effects.ReportProblem (getProblems, runReportProblem)
import Fixtures.Deslop.Module (edgeTo, mkModule)
import Fixtures.Deslop.Problem (BrokenRule (..), importedBy, violationOf)
import Fixtures.Deslop.Rule.Book (rulebookFrom, rulebookOf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import TestUtils (requireRight)

--------------------------------------------------------------------------------
-- Rulebooks
--
-- Written as their authors would write them and loaded through the loader, so
-- every rule here reaches the enforcer exactly as a real rulebook does.
--------------------------------------------------------------------------------

testRulebook :: Rulebook
testRulebook =
    rulebookFrom
                "test-rulebook"
                [ [ "id: no-forbids-import"
                    , "description: 'Forbids modules must not be imported.'"
                    , "target: '@/components/**'"
                    , "forbids:"
                    , "  - import: '@/forbids/**'"
                    , "    transitive: false"
                    , "fix: 'Remove the import'"
                    ]
                ]

testTransitiveRulebook :: Rulebook
testTransitiveRulebook =
    rulebookFrom
                "test-rulebook"
                [ [ "id: no-forbids-import"
                    , "description: 'Forbids modules must not be transitively imported.'"
                    , "target: '@/components/**'"
                    , "forbids:"
                    , "  - import: '@/forbids/**'"
                    , "    transitive: true"
                    , "fix: 'Remove the import'"
                    ]
                ]

runTest :: Module -> IO [Problem]
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

runTransitiveTestWith :: [Rulebook] -> [Module] -> Module -> IO [Problem]
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

runTransitiveTest :: [Module] -> Module -> IO [Problem]
runTransitiveTest = runTransitiveTestWith [testTransitiveRulebook]

domainRulebook :: Rulebook
domainRulebook =
    rulebookFrom
                "domain-rules"
                [ [ "id: no-react-in-domain"
                    , "description: 'Domain layer must not depend on React.'"
                    , "target: '@/domain/**'"
                    , "forbids:"
                    , "  - import: 'react'"
                    , "    transitive: true"
                    , "fix: 'Move React dependencies out of the domain layer.'"
                    ]
                ]

existsRulebook :: Rulebook
existsRulebook =
    rulebookFrom
                "exists-rules"
                [ [ "id: requires-spec"
                    , "description: 'Every ViewModel must have a spec file.'"
                    , "target: '@/features/**/use{{FileName}}ViewModel'"
                    , "exists:"
                    , "  - module: '{{TARGET_DIR}}/use{{FileName}}ViewModel.spec'"
                    , "fix: 'Create the spec file.'"
                    ]
                ]

runExistsTest :: [Rulebook] -> [Module] -> Module -> IO (Either DeslopError [Problem])
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
    rulebookFrom
                "uses-rules"
                [ [ "id: container-wires-state-event"
                    , "description: 'Containers must wire their StateEvent.'"
                    , "target: '@/features/**/{{FileName}}Container'"
                    , "uses:"
                    , "  - import: '{{TARGET_DIR}}/{{FileName}}StateEvent'"
                    , "    transitive: false"
                    , "fix: 'Import the StateEvent.'"
                    ]
                ]

runUsesTest :: [Module] -> Module -> IO (Either DeslopError [Problem])
runUsesTest allModules m =
    runEff
        . runErrorNoCallStack @DeslopError
        . runReportProblem
        . runReader (buildModuleGraph allModules)
        . runReader [usesRulebook]
        $ do
            enforceRulebooks m
            getProblems

--------------------------------------------------------------------------------
-- Several variables at once
--------------------------------------------------------------------------------

{- | Three variables, captured from one target and spent across every kind of
clause. Each rule has its own target segment so that a violation names the rule
that produced it without any of the others joining in.
-}
multiVarRulebook :: Rulebook
multiVarRulebook =
    rulebookFrom
                "multi-var-rules"
                [ [ "id: view-keeps-out-of-another-internal"
                    , "description: 'A View may not reach an internal module of its own provider and service.'"
                    , "target: '@/direct/{{provider-name}}/{{service-type}}/{{FileName}}View'"
                    , "forbids:"
                    , "  - import: '@/internal/{{provider-name}}/{{ServiceType}}{{FileName}}Secret'"
                    , "    transitive: false"
                    , "fix: 'Do not import that internal module.'"
                    ]
                , [ "id: view-uses-its-own-service"
                    , "description: 'A View must import the service named by all three of its variables.'"
                    , "target: '@/uses/{{provider-name}}/{{service-type}}/{{FileName}}View'"
                    , "uses:"
                    , "  - import: '@/services/{{provider-name}}/{{service-type}}-{{file-name}}'"
                    , "    transitive: false"
                    , "fix: 'Import your own provider''s service.'"
                    ]
                , [ "id: view-reaches-its-service-eventually"
                    , "description: 'A View must reach its own service, directly or through an intermediary.'"
                    , "target: '@/reaches/{{provider-name}}/{{service-type}}/{{FileName}}View'"
                    , "uses:"
                    , "  - import: '@/services/{{provider-name}}/{{service-type}}-{{file-name}}'"
                    , "    transitive: true"
                    , "fix: 'Reach your own provider''s service.'"
                    ]
                , [ "id: view-never-reaches-another-internal"
                    , "description: 'A View must not reach an internal module, at any depth.'"
                    , "target: '@/deep/{{provider-name}}/{{service-type}}/{{FileName}}View'"
                    , "forbids:"
                    , "  - import: '@/internal/{{provider-name}}/{{ServiceType}}/**'"
                    , "    transitive: true"
                    , "fix: 'Break the chain into that internal folder.'"
                    ]
                , [ "id: view-has-a-spec-named-by-all-three"
                    , "description: 'A View''s spec is named by its provider, its service type and its own name.'"
                    , "target: '@/exists/{{provider-name}}/{{service-type}}/{{FileName}}View'"
                    , "exists:"
                    , "  - module: '@/specs/{{provider-name}}/{{ServiceType}}{{FileName}}View.spec'"
                    , "fix: 'Add the spec.'"
                    ]
                , [ "id: widget-has-a-config"
                    , "description: 'A Widget''s config is its own name in kebab-case.'"
                    , "target: '@/widgets/{{FileName}}Widget'"
                    , "exists:"
                    , "  - module: '@/config/{{file-name}}'"
                    , "fix: 'Add the config.'"
                    ]
                , [ "id: provider-view-has-a-config"
                    , "description: 'A View named after its folder has a config under that name.'"
                    , "target: '@/pair/{{provider-name}}/{{ProviderName}}View'"
                    , "exists:"
                    , "  - module: '@/config/{{provider-name}}'"
                    , "fix: 'Add the config.'"
                    ]
                , [ "id: use-case-has-a-view-model"
                    , "description: 'A use case is driven by a ViewModel named after it.'"
                    , "target: '@/app/{{use-case-name}}/{{UseCaseName}}UseCase'"
                    , "exists:"
                    , "  - module: '{{TARGET_DIR}}/use{{UseCaseName}}ViewModel'"
                    , "fix: 'Add the ViewModel.'"
                    ]
                , [ "id: view-imports-only-its-own-service"
                    , "description: 'A View may import services, but only the one all three variables name.'"
                    , "target: '@/allows/{{provider-name}}/{{service-type}}/{{FileName}}View'"
                    , "forbids:"
                    , "  - import: '@/services/**'"
                    , "    transitive: false"
                    , "allows:"
                    , "  - import: '@/services/{{provider-name}}/{{service-type}}-{{file-name}}'"
                    , "fix: 'Import only your own provider''s service.'"
                    ]
                ]

runMultiVarTest :: [Module] -> Module -> IO [Problem]
runMultiVarTest = runTransitiveTestWith [multiVarRulebook]

--------------------------------------------------------------------------------
-- Variables in a rule's prose
--------------------------------------------------------------------------------

{- | A rule whose @description@ and @fix@ speak about the match rather than
about the pattern. The unbound @{{provider-name}}@ is deliberate: prose is not
validated at load, so a name nothing captured has to survive being reported.
-}
proseRulebook :: Rulebook
proseRulebook =
    rulebookFrom
                "prose-rules"
                [ [ "id: container-wires-state-event"
                    , "description: '{{FileName}}Container must wire {{FileName}}StateEvent.'"
                    , "target: '@/features/**/{{FileName}}Container'"
                    , "uses:"
                    , "  - import: '{{TARGET_DIR}}/{{FileName}}StateEvent'"
                    , "    transitive: false"
                    , "fix: 'Import {{FileName}}StateEvent from {{TARGET_DIR}}, or move it to {{TARGET_DIR}}/{{file-name}}. Not {{provider-name}}.'"
                    ]
                ]

runProseTest :: Module -> IO [Problem]
runProseTest m = runTransitiveTestWith [proseRulebook] [m] m

fixesOf :: [Problem] -> [Text]
fixesOf = mapMaybe fixOf
  where
    fixOf RuleViolation {fix = f} = Just f
    fixOf _ = Nothing

proseSpec :: Spec
proseSpec = describe "variables in a rule's prose" $ do
    let violation = runProseTest (mkModule "@/features/checkout/PaymentContainer" [])

    it "substitutes the captured variables into the fix" $
        fixesOf
            <$> violation
                `shouldReturn` [ "Import PaymentStateEvent from @/features/checkout, "
                                    <> "or move it to @/features/checkout/payment. Not {{provider-name}}."
                               ]

    it "substitutes the captured variables into the prose" $
        prosesOf
            <$> violation
                `shouldReturn` ["PaymentContainer must wire PaymentStateEvent."]

    it "substitutes the captured variables into what the rule requires" $
        kindsOf
            <$> violation
                `shouldReturn` [MissingUse {requiredImport = "@/features/checkout/PaymentStateEvent", transitive = False}]

prosesOf :: [Problem] -> [Text]
prosesOf = mapMaybe proseOf
  where
    proseOf RuleViolation {prose = p} = Just p
    proseOf _ = Nothing

kindsOf :: [Problem] -> [ViolationKind]
kindsOf = mapMaybe kindOf
  where
    kindOf RuleViolation {kind = k} = Just k
    kindOf _ = Nothing

ruleIdsOf :: [Problem] -> [Text]
ruleIdsOf = mapMaybe ruleIdOf
  where
    ruleIdOf RuleViolation {rule = RuleId r} = Just r
    ruleIdOf _ = Nothing

multiVariableSpec :: Spec
multiVariableSpec = describe "several variables in one rule" $ do
    describe "forbids, direct" $ do
        it "reports when every variable lines up" $ do
            let m = mkModule "@/direct/stripe-connect/payment/CheckoutView" ["@/internal/stripe-connect/PaymentCheckoutSecret"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` ["view-keeps-out-of-another-internal"]

        it "does not report when one variable differs" $ do
            let m = mkModule "@/direct/stripe-connect/payment/CheckoutView" ["@/internal/paypal/PaymentCheckoutSecret"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` []

        it "reports an acronym spelling of any of the variables" $ do
            -- Forbidding clauses widen, so no spelling of PaymentCheckout evades it.
            let m = mkModule "@/direct/stripe-connect/payment/CheckoutView" ["@/internal/stripe-connect/PAYMENTCHECKOUTSecret"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` ["view-keeps-out-of-another-internal"]

    describe "uses, direct" $ do
        it "is satisfied when every variable lines up" $ do
            let m = mkModule "@/uses/stripe-connect/payment/CheckoutView" ["@/services/stripe-connect/payment-checkout"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` []

        it "reports when one variable differs" $ do
            let m = mkModule "@/uses/stripe-connect/payment/CheckoutView" ["@/services/paypal/payment-checkout"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` ["view-uses-its-own-service"]

        it "reports when a variable is spelled a way a requiring clause does not accept" $ do
            let m = mkModule "@/uses/stripe-connect/payment/CheckoutView" ["@/services/stripe-connect/PAYMENT-checkout"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` ["view-uses-its-own-service"]

    describe "uses, transitive" $ do
        it "is satisfied when the service is reached through an intermediary" $ do
            let view = mkModule "@/reaches/stripe-connect/payment/CheckoutView" ["@/lib/relay"]
            let relay = mkModule "@/lib/relay" ["@/services/stripe-connect/payment-checkout"]
            let service = mkModule "@/services/stripe-connect/payment-checkout" []
            ruleIdsOf <$> runMultiVarTest [view, relay, service] view `shouldReturn` []

        it "reports when the intermediary reaches another provider's service" $ do
            let view = mkModule "@/reaches/stripe-connect/payment/CheckoutView" ["@/lib/relay"]
            let relay = mkModule "@/lib/relay" ["@/services/paypal/payment-checkout"]
            let service = mkModule "@/services/paypal/payment-checkout" []
            ruleIdsOf <$> runMultiVarTest [view, relay, service] view
                `shouldReturn` ["view-reaches-its-service-eventually"]

    describe "forbids, transitive" $ do
        it "reports an internal module reached through an intermediary" $ do
            let view = mkModule "@/deep/stripe-connect/payment/CheckoutView" ["@/lib/relay"]
            let relay = mkModule "@/lib/relay" ["@/internal/stripe-connect/Payment/secret"]
            let secret = mkModule "@/internal/stripe-connect/Payment/secret" []
            ruleIdsOf <$> runMultiVarTest [view, relay, secret] view
                `shouldReturn` ["view-never-reaches-another-internal"]

        it "does not report when the reachable internal belongs to another provider" $ do
            let view = mkModule "@/deep/stripe-connect/payment/CheckoutView" ["@/lib/relay"]
            let relay = mkModule "@/lib/relay" ["@/internal/paypal/Payment/secret"]
            let secret = mkModule "@/internal/paypal/Payment/secret" []
            ruleIdsOf <$> runMultiVarTest [view, relay, secret] view `shouldReturn` []

        it "reports an acronym spelling reached through an intermediary" $ do
            let view = mkModule "@/deep/http-client/db-sync/CheckoutView" ["@/lib/relay"]
            let relay = mkModule "@/lib/relay" ["@/internal/http-client/DBSync/secret"]
            let secret = mkModule "@/internal/http-client/DBSync/secret" []
            ruleIdsOf <$> runMultiVarTest [view, relay, secret] view
                `shouldReturn` ["view-never-reaches-another-internal"]

    describe "exists" $ do
        it "is satisfied when the module all three variables name is in the graph" $ do
            let view = mkModule "@/exists/stripe-connect/payment/CheckoutView" []
            let spec' = mkModule "@/specs/stripe-connect/PaymentCheckoutView.spec" []
            ruleIdsOf <$> runMultiVarTest [view, spec'] view `shouldReturn` []

        it "reports the exact module it wanted when it is missing" $ do
            let view = mkModule "@/exists/stripe-connect/payment/CheckoutView" []
            problems <- runMultiVarTest [view] view
            ruleIdsOf problems `shouldBe` ["view-has-a-spec-named-by-all-three"]
            kindsOf problems
                `shouldSatisfy` elem MissingModule {requiredModule = moduleNameUnsafe "@/specs/stripe-connect/PaymentCheckoutView.spec"}

        it "is not satisfied by another provider's spec" $ do
            let view = mkModule "@/exists/stripe-connect/payment/CheckoutView" []
            let wrong = mkModule "@/specs/paypal/PaymentCheckoutView.spec" []
            ruleIdsOf <$> runMultiVarTest [view, wrong] view
                `shouldReturn` ["view-has-a-spec-named-by-all-three"]

        it "is not satisfied by a spec whose service type differs" $ do
            let view = mkModule "@/exists/stripe-connect/payment/CheckoutView" []
            let wrong = mkModule "@/specs/stripe-connect/PayoutCheckoutView.spec" []
            ruleIdsOf <$> runMultiVarTest [view, wrong] view
                `shouldReturn` ["view-has-a-spec-named-by-all-three"]

        it "does not apply to a module the target does not match" $ do
            let notAView = mkModule "@/exists/stripe-connect/payment/CheckoutContainer" []
            ruleIdsOf <$> runMultiVarTest [notAView] notAView `shouldReturn` []

        describe "when the name was captured as an acronym" $ do
            it "reads a run of capitals as one word" $ do
                let widget = mkModule "@/widgets/DBConnectionWidget" []
                let config = mkModule "@/config/db-connection" []
                ruleIdsOf <$> runMultiVarTest [widget, config] widget `shouldReturn` []

            it "asks for a module that cannot exist for two adjacent acronyms - a documented limitation" $ do
                let widget = mkModule "@/widgets/AWSS3Widget" []
                let config = mkModule "@/config/aws-s3" []
                problems <- runMultiVarTest [widget, config] widget
                ruleIdsOf problems `shouldBe` ["widget-has-a-config"]
                kindsOf problems
                    `shouldSatisfy` elem MissingModule {requiredModule = moduleNameUnsafe "@/config/awss3"}

            it "is exact when the target names the folder in kebab-case too" $ do
                let view = mkModule "@/pair/http-client/HTTPClientView" []
                let config = mkModule "@/config/http-client" []
                ruleIdsOf <$> runMultiVarTest [view, config] view `shouldReturn` []

            it "reports the kebab-case name when that config is missing" $ do
                let view = mkModule "@/pair/aws-s3/AWSS3View" []
                problems <- runMultiVarTest [view] view
                ruleIdsOf problems `shouldBe` ["provider-view-has-a-config"]
                kindsOf problems
                    `shouldSatisfy` elem MissingModule {requiredModule = moduleNameUnsafe "@/config/aws-s3"}

            it "does not apply when the folder and the file are different names" $ do
                let view = mkModule "@/pair/stripe-connect/PaypalView" []
                ruleIdsOf <$> runMultiVarTest [view] view `shouldReturn` []

        describe "with a name of three or more words" $ do
            it "is satisfied by the ViewModel named after the use case" $ do
                let useCase = mkModule "@/app/archive-order/ArchiveOrderUseCase" []
                let viewModel = mkModule "@/app/archive-order/useArchiveOrderViewModel" []
                ruleIdsOf <$> runMultiVarTest [useCase, viewModel] useCase `shouldReturn` []

            it "reports the affixed module it wanted when it is missing" $ do
                let useCase = mkModule "@/app/archive-order/ArchiveOrderUseCase" []
                problems <- runMultiVarTest [useCase] useCase
                ruleIdsOf problems `shouldBe` ["use-case-has-a-view-model"]
                kindsOf problems
                    `shouldSatisfy` elem MissingModule {requiredModule = moduleNameUnsafe "@/app/archive-order/useArchiveOrderViewModel"}

            it "keeps the captured acronym spelling in the module it asks for" $ do
                let useCase = mkModule "@/app/http-client-retry/HTTPClientRetryUseCase" []
                problems <- runMultiVarTest [useCase] useCase
                kindsOf problems
                    `shouldSatisfy` elem MissingModule {requiredModule = moduleNameUnsafe "@/app/http-client-retry/useHTTPClientRetryViewModel"}

    describe "allows over a broad forbids" $ do
        it "exempts the service all three variables name" $ do
            let m = mkModule "@/allows/stripe-connect/payment/CheckoutView" ["@/services/stripe-connect/payment-checkout"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` []

        it "does not exempt another provider's service" $ do
            let m = mkModule "@/allows/stripe-connect/payment/CheckoutView" ["@/services/paypal/payment-checkout"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` ["view-imports-only-its-own-service"]

        it "does not exempt a spelling the allows clause did not name" $ do
            -- An allows clause narrows: exempting more than was written would
            -- silence a real violation.
            let m = mkModule "@/allows/stripe-connect/payment/CheckoutView" ["@/services/stripe-connect/PAYMENT-checkout"]
            ruleIdsOf <$> runMultiVarTest [m] m `shouldReturn` ["view-imports-only-its-own-service"]

spec :: Spec
spec = describe "Deslop.Rule.Enforcer" $ do
    multiVariableSpec
    proseSpec
    describe "forbids imports" $ do
        it "no violations" $ do
            let m = mkModule "@/components/Button" ["react"]
            problems <- runTest m
            problems `shouldBe` []

        it "direct import violation" $ do
            let m = mkModule "@/components/Button" ["@/forbids/module"]
            problems <- runTest m
            problems
                `shouldBe` [ violationOf noForbidsImport "@/components/Button" (importedBy m "@/forbids/module")
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
                `shouldBe` [ violationOf noForbidsImportTransitively "@/components/Button" (TransitiveImport {chain = moduleNameUnsafe "@/components/Button" :| [moduleNameUnsafe "@/forbids/store"], firstImport = Just (edgeTo button "@/forbids/store"), alsoReached = []})
                           ]

        it "multi-hop transitive violation" $ do
            let button = mkModule "@/components/Button" ["@/lib/util"]
                util = mkModule "@/lib/util" ["@/forbids/store"]
                forbids = mkModule "@/forbids/store" []
            problems <- runTransitiveTest [button, util, forbids] button
            problems
                `shouldBe` [ violationOf noForbidsImportTransitively "@/components/Button" (TransitiveImport {chain = moduleNameUnsafe "@/components/Button" :| [moduleNameUnsafe "@/lib/util", moduleNameUnsafe "@/forbids/store"], firstImport = Just (edgeTo button "@/lib/util"), alsoReached = []})
                           ]

        -- Two forbidden modules behind two different imports are two separate
        -- edits, so the enforcer reports both and "Deslop.Problem.Shrinker"
        -- keeps both. Problems of one rule sort by their import chain.
        it "reports multiple reachable forbids modules" $ do
            let button = mkModule "@/components/Button" ["@/lib/util", "@/lib/helpers"]
                util = mkModule "@/lib/util" ["@/forbids/storeA"]
                helpers = mkModule "@/lib/helpers" ["@/forbids/storeB"]
                forbidsA = mkModule "@/forbids/storeA" []
                forbidsB = mkModule "@/forbids/storeB" []
            problems <- runTransitiveTest [button, util, helpers, forbidsA, forbidsB] button
            problems
                `shouldBe` [ violationOf noForbidsImportTransitively "@/components/Button" (TransitiveImport {chain = moduleNameUnsafe "@/components/Button" :| [moduleNameUnsafe "@/lib/helpers", moduleNameUnsafe "@/forbids/storeB"], firstImport = Just (edgeTo button "@/lib/helpers"), alsoReached = []})
                           , violationOf noForbidsImportTransitively "@/components/Button" (TransitiveImport {chain = moduleNameUnsafe "@/components/Button" :| [moduleNameUnsafe "@/lib/util", moduleNameUnsafe "@/forbids/storeA"], firstImport = Just (edgeTo button "@/lib/util"), alsoReached = []})
                           ]

        it "domain module must not transitively import react" $ do
            let useCase = mkModule "@/domain/LoginUseCase" ["@/domain/UserRepository"]
                repo = mkModule "@/domain/UserRepository" ["@/infrastructure/HttpClient"]
                http = mkModule "@/infrastructure/HttpClient" ["react"]
                react = mkModule "react" []
            problems <- runTransitiveTestWith [domainRulebook] [useCase, repo, http, react] useCase
            problems
                `shouldBe` [ violationOf noReactInDomain "@/domain/LoginUseCase" (TransitiveImport {chain = moduleNameUnsafe "@/domain/LoginUseCase" :| [moduleNameUnsafe "@/domain/UserRepository", moduleNameUnsafe "@/infrastructure/HttpClient", moduleNameUnsafe "react"], firstImport = Just (edgeTo useCase "@/domain/UserRepository"), alsoReached = []})
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
                    [ violationOf requiresSpec "@/features/home/useHomeViewModel" (MissingModule {requiredModule = moduleNameUnsafe "@/features/home/useHomeViewModel.spec"})
                    ]

        it "does not report a violation for a module that does not match the target" $ do
            let notAViewModel = mkModule "@/features/home/HomeView" []
            result <- runExistsTest [existsRulebook] [notAViewModel] notAViewModel
            result `shouldBe` Right []

        {- An exists pattern holding a wildcard has no single module to look up,
        and that is now settled when the rulebook is compiled - so the enforcer
        can never be handed one, and the rule never reaches a run at all. -}
        it "never reaches enforcement, because a wildcard exists pattern does not compile" $ do
            let wildcardRulebook =
                    rulebookOf
                            "bad-rules"
                            [ [ "id: wildcard-exists"
                                , "description: 'wildcard exists'"
                                , "target: '@/features/**/*'"
                                , "exists:"
                                , "  - module: '{{TARGET_DIR}}/**/*.spec'"
                                , "fix: 'Fix it.'"
                                ]
                            ]

            wildcardRulebook `shouldSatisfy` \case
                Left (UncompilablePatterns errors) -> fmap (.field) errors == one ExistsField
                Right _ -> False
                Left (UnreadableYaml _) -> False

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
                    [ violationOf containerWiresStateEvent "@/features/home/HomeContainer" (MissingUse {requiredImport = "@/features/home/HomeStateEvent", transitive = False})
                    ]

        it "reports a violation when module has no imports at all" $ do
            let container = mkModule "@/features/home/HomeContainer" []
            result <- runUsesTest [container] container
            result
                `shouldBe` Right
                    [ violationOf containerWiresStateEvent "@/features/home/HomeContainer" (MissingUse {requiredImport = "@/features/home/HomeStateEvent", transitive = False})
                    ]

        it "no violation for a module that does not match the target" $ do
            let notAContainer = mkModule "@/features/home/HomeView" []
            result <- runUsesTest [notAContainer] notAContainer
            result `shouldBe` Right []

        it "reports one violation per unmatched uses pattern" $ do
            let multiUsesRulebook =
                    rulebookFrom
                                "uses-rules"
                                [ [ "id: container-wires-all"
                                    , "description: 'Containers must wire all their dependencies.'"
                                    , "target: '@/features/**/{{FileName}}Container'"
                                    , "uses:"
                                    , "  - import: '{{TARGET_DIR}}/{{FileName}}StateEvent'"
                                    , "    transitive: false"
                                    , "  - import: '{{TARGET_DIR}}/{{FileName}}View'"
                                    , "    transitive: false"
                                    , "fix: 'Wire the Container.'"
                                    ]
                                ]
                container = mkModule "@/features/home/HomeContainer" []
            result <- runExistsTest [multiUsesRulebook] [container] container
            result
                `shouldBe` Right
                    [ violationOf containerWiresAll "@/features/home/HomeContainer" (MissingUse {requiredImport = "@/features/home/HomeStateEvent", transitive = False})
                    , violationOf containerWiresAll "@/features/home/HomeContainer" (MissingUse {requiredImport = "@/features/home/HomeView", transitive = False})
                    ]

        it "wildcard uses pattern matches any qualifying import" $ do
            let wildcardUsesRulebook =
                    rulebookFrom
                                "uses-rules"
                                [ [ "id: page-uses-container"
                                    , "description: 'page uses container'"
                                    , "target: '@/app/**/page'"
                                    , "uses:"
                                    , "  - import: '@/features/**/*Container'"
                                    , "    transitive: false"
                                    , "fix: 'Import a Container.'"
                                    ]
                                ]
                page = mkModule "@/app/home/page" ["@/features/home/HomeContainer"]
            result <- runExistsTest [wildcardUsesRulebook] [page] page
            result `shouldBe` Right []

    describe "transitive uses enforcement" $ do
        let usesTransitiveRulebook =
                rulebookFrom
                            "uses-rules"
                            [ [ "id: container-wires-state-event-transitively"
                                , "description: 'Containers must transitively wire their StateEvent.'"
                                , "target: '@/features/**/{{FileName}}Container'"
                                , "uses:"
                                , "  - import: '{{TARGET_DIR}}/{{FileName}}StateEvent'"
                                , "    transitive: true"
                                , "fix: 'Import the StateEvent.'"
                                ]
                            ]
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
                `shouldBe` [ violationOf containerWiresStateEventTransitively "@/features/home/HomeContainer" (MissingUse {requiredImport = "@/features/home/HomeStateEvent", transitive = True})
                           ]

        it "reports a violation when module has no imports at all" $ do
            let container = mkModule "@/features/home/HomeContainer" []
            problems <- runUsesTransitiveTest [container] container
            problems
                `shouldBe` [ violationOf containerWiresStateEventTransitively "@/features/home/HomeContainer" (MissingUse {requiredImport = "@/features/home/HomeStateEvent", transitive = True})
                           ]

        it "no violation for a module that does not match the target" $ do
            let notAContainer = mkModule "@/features/home/HomeView" []
            problems <- runUsesTransitiveTest [notAContainer] notAContainer
            problems `shouldBe` []

        it "wildcard transitive uses pattern matches a reachable module" $ do
            let wildcardTransitiveRulebook =
                    rulebookFrom
                                "uses-rules"
                                [ [ "id: domain-must-use-logger"
                                    , "description: 'domain must use logger'"
                                    , "target: '@/domain/**'"
                                    , "uses:"
                                    , "  - import: '@/infrastructure/**/*Logger'"
                                    , "    transitive: true"
                                    , "fix: 'Ensure a logger is used.'"
                                    ]
                                ]
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
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: no-forbidden-imports"
                                    , "description: 'Forbidden imports not allowed.'"
                                    , "target: '@/components/**'"
                                    , "forbids:"
                                    , "  - import: '@/forbids/**'"
                                    , "    transitive: false"
                                    , "allows:"
                                    , "  - import: '@/allowed/**'"
                                    , "fix: 'Remove the import.'"
                                    ]
                                ]
                sharedOnlyRulebook =
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: shared-only"
                                    , "description: 'Only shared imports allowed.'"
                                    , "target: '@/components/**'"
                                    , "forbids:"
                                    , "  - import: '**'"
                                    , "    transitive: false"
                                    , "allows:"
                                    , "  - import: '@/shared/**'"
                                    , "fix: 'Use shared modules only.'"
                                    ]
                                ]
                domainPurityRulebook =
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: domain-purity"
                                    , "description: 'Domain modules may only import from domain or shared layers.'"
                                    , "target: '@/domain/**'"
                                    , "forbids:"
                                    , "  - import: '**'"
                                    , "    transitive: false"
                                    , "allows:"
                                    , "  - import: '@/domain/**'"
                                    , "  - import: '@/shared/**'"
                                    , "fix: 'Move the dependency to the correct layer.'"
                                    ]
                                ]

            it "no violation when import matches allows pattern" $ do
                let m = mkModule "@/components/Button" ["@/allowed/utils"]
                problems <- runTransitiveTestWith [directAllowsRulebook] [m] m
                problems `shouldBe` []

            it "violation when import matches forbids but not allows" $ do
                let m = mkModule "@/components/Button" ["@/forbids/store"]
                problems <- runTransitiveTestWith [directAllowsRulebook] [m] m
                problems
                    `shouldBe` [ violationOf noForbiddenImports "@/components/Button" (importedBy m "@/forbids/store")
                               ]

            it "no violation when forbids all (**) but import matches allows (shared module exception)" $ do
                let m = mkModule "@/components/Button" ["@/shared/utils"]
                problems <- runTransitiveTestWith [sharedOnlyRulebook] [m] m
                problems `shouldBe` []

            it "violation when forbids all (**) and import does not match any allows pattern" $ do
                let m = mkModule "@/components/Button" ["react"]
                problems <- runTransitiveTestWith [sharedOnlyRulebook] [m] m
                problems
                    `shouldBe` [ violationOf sharedOnly "@/components/Button" (importedBy m "react")
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
                    `shouldBe` [ violationOf domainPurity "@/domain/LoginUseCase" (importedBy m "@/infrastructure/HttpClient")
                               ]

            it "violation: domain purity - external library import blocked" $ do
                let m = mkModule "@/domain/LoginUseCase" ["react"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems
                    `shouldBe` [ violationOf domainPurity "@/domain/LoginUseCase" (importedBy m "react")
                               ]

            it "violation: domain purity - deeply nested domain module imports infrastructure layer" $ do
                let m = mkModule "@/domain/user/login/LoginUseCase" ["@/infrastructure/db/UserDbRepository"]
                problems <- runTransitiveTestWith [domainPurityRulebook] [m] m
                problems
                    `shouldBe` [ violationOf domainPurity "@/domain/user/login/LoginUseCase" (importedBy m "@/infrastructure/db/UserDbRepository")
                               ]

        describe "transitive forbids with allows" $ do
            let storeAllowsRulebook =
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: no-transitive-store"
                                    , "description: 'Components must not transitively import store modules.'"
                                    , "target: '@/components/**'"
                                    , "forbids:"
                                    , "  - import: '@/store/**'"
                                    , "    transitive: true"
                                    , "allows:"
                                    , "  - import: '@/store/ui-store'"
                                    , "fix: 'Remove the transitive store import.'"
                                    ]
                                ]
                domainPurityTransitiveRulebook =
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: domain-purity-transitive"
                                    , "description: 'Domain must not transitively reach non-domain/shared modules.'"
                                    , "target: '@/domain/**'"
                                    , "forbids:"
                                    , "  - import: '**'"
                                    , "    transitive: true"
                                    , "allows:"
                                    , "  - import: '@/domain/**'"
                                    , "  - import: '@/shared/**'"
                                    , "fix: 'Keep domain pure.'"
                                    ]
                                ]

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
                    `shouldBe` [ violationOf noTransitiveStore "@/components/Button" (TransitiveImport {chain = moduleNameUnsafe "@/components/Button" :| [moduleNameUnsafe "@/lib/hooks", moduleNameUnsafe "@/store/app-store"], firstImport = Just (edgeTo button "@/lib/hooks"), alsoReached = []})
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
                    `shouldBe` [ violationOf domainPurityTransitive "@/domain/LoginUseCase" (TransitiveImport {chain = moduleNameUnsafe "@/domain/LoginUseCase" :| [moduleNameUnsafe "@/domain/AuthService", moduleNameUnsafe "@/infrastructure/HttpClient"], firstImport = Just (edgeTo useCase "@/domain/AuthService"), alsoReached = []})
                               ]

        describe "cross-feature isolation" $ do
            -- {{TARGET_DIR}} expands to the directory of the matched module.
            -- e.g. @/features/home/HomeContainer → {{TARGET_DIR}} = @/features/home
            --      @/features/home/data/HomeRepository → {{TARGET_DIR}} = @/features/home/data
            -- This enforces directory-level isolation: a module may only import from its
            -- own directory (and any subdirectories, since ** spans segments).
            let dirIsolationRulebook =
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: dir-isolation"
                                    , "description: 'Feature modules may only import from their own directory.'"
                                    , "target: '@/features/**'"
                                    , "forbids:"
                                    , "  - import: '**'"
                                    , "    transitive: false"
                                    , "allows:"
                                    , "  - import: '{{TARGET_DIR}}/**'"
                                    , "fix: 'Keep imports within the same directory or extract to shared.'"
                                    ]
                                ]
                -- Relaxed variant: also permits @/shared/** imports
                dirIsolationWithSharedRulebook =
                    rulebookFrom
                                "test-rulebook"
                                [ [ "id: dir-isolation-shared"
                                    , "description: 'Feature modules may only import from their own directory or shared.'"
                                    , "target: '@/features/**'"
                                    , "forbids:"
                                    , "  - import: '**'"
                                    , "    transitive: false"
                                    , "allows:"
                                    , "  - import: '{{TARGET_DIR}}/**'"
                                    , "  - import: '@/shared/**'"
                                    , "fix: 'Keep imports within the same directory or use shared modules.'"
                                    ]
                                ]

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
                    `shouldBe` [ violationOf dirIsolation "@/features/home/HomeContainer" (importedBy m "@/features/auth/AuthService")
                               ]

            it "violation: top-level feature module imports external library" $ do
                let m = mkModule "@/features/home/HomeContainer" ["react"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ violationOf dirIsolation "@/features/home/HomeContainer" (importedBy m "react")
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
                    `shouldBe` [ violationOf dirIsolation "@/features/home/data/HomeRepository" (importedBy m "@/features/home/HomeService")
                               ]

            it "violation (edge case): nested module cannot import from sibling directory within same feature" $ do
                -- @/features/home/data/** does not match @/features/home/ui/HomeButton
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/home/ui/HomeButton"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ violationOf dirIsolation "@/features/home/data/HomeRepository" (importedBy m "@/features/home/ui/HomeButton")
                               ]

            it "violation: nested module imports from a completely different feature" $ do
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/auth/data/AuthRepository"]
                problems <- runTransitiveTestWith [dirIsolationRulebook] [m] m
                problems
                    `shouldBe` [ violationOf dirIsolation "@/features/home/data/HomeRepository" (importedBy m "@/features/auth/data/AuthRepository")
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
                    `shouldBe` [ violationOf dirIsolationShared "@/features/home/HomeContainer" (importedBy m "@/features/auth/AuthService")
                               ]

            it "violation: relaxed rule - external library still blocked" $ do
                let m = mkModule "@/features/home/HomeContainer" ["react"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems
                    `shouldBe` [ violationOf dirIsolationShared "@/features/home/HomeContainer" (importedBy m "react")
                               ]

            it "violation: relaxed rule - nested module's parent-dir import still blocked" $ do
                -- Even with shared allowed, the asymmetric edge case still holds:
                -- nested module cannot reach its own parent directory
                let m = mkModule "@/features/home/data/HomeRepository" ["@/features/home/HomeService"]
                problems <- runTransitiveTestWith [dirIsolationWithSharedRulebook] [m] m
                problems
                    `shouldBe` [ violationOf dirIsolationShared "@/features/home/data/HomeRepository" (importedBy m "@/features/home/HomeService")
                               ]

--------------------------------------------------------------------------------
-- The Rules the violations above are reported against
--------------------------------------------------------------------------------

noForbidsImport :: BrokenRule
noForbidsImport =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "no-forbids-import"
        , ruleProse = "Forbids modules must not be imported."
        , ruleFix = "Remove the import"
        }

noForbidsImportTransitively :: BrokenRule
noForbidsImportTransitively =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "no-forbids-import"
        , ruleProse = "Forbids modules must not be transitively imported."
        , ruleFix = "Remove the import"
        }

noReactInDomain :: BrokenRule
noReactInDomain =
    BrokenRule
        { rulebookId = RulebookId "domain-rules"
        , ruleId = RuleId "no-react-in-domain"
        , ruleProse = "Domain layer must not depend on React."
        , ruleFix = "Move React dependencies out of the domain layer."
        }

requiresSpec :: BrokenRule
requiresSpec =
    BrokenRule
        { rulebookId = RulebookId "exists-rules"
        , ruleId = RuleId "requires-spec"
        , ruleProse = "Every ViewModel must have a spec file."
        , ruleFix = "Create the spec file."
        }

containerWiresStateEvent :: BrokenRule
containerWiresStateEvent =
    BrokenRule
        { rulebookId = RulebookId "uses-rules"
        , ruleId = RuleId "container-wires-state-event"
        , ruleProse = "Containers must wire their StateEvent."
        , ruleFix = "Import the StateEvent."
        }

containerWiresAll :: BrokenRule
containerWiresAll =
    BrokenRule
        { rulebookId = RulebookId "uses-rules"
        , ruleId = RuleId "container-wires-all"
        , ruleProse = "Containers must wire all their dependencies."
        , ruleFix = "Wire the Container."
        }

containerWiresStateEventTransitively :: BrokenRule
containerWiresStateEventTransitively =
    BrokenRule
        { rulebookId = RulebookId "uses-rules"
        , ruleId = RuleId "container-wires-state-event-transitively"
        , ruleProse = "Containers must transitively wire their StateEvent."
        , ruleFix = "Import the StateEvent."
        }

noForbiddenImports :: BrokenRule
noForbiddenImports =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "no-forbidden-imports"
        , ruleProse = "Forbidden imports not allowed."
        , ruleFix = "Remove the import."
        }

sharedOnly :: BrokenRule
sharedOnly =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "shared-only"
        , ruleProse = "Only shared imports allowed."
        , ruleFix = "Use shared modules only."
        }

domainPurity :: BrokenRule
domainPurity =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "domain-purity"
        , ruleProse = "Domain modules may only import from domain or shared layers."
        , ruleFix = "Move the dependency to the correct layer."
        }

noTransitiveStore :: BrokenRule
noTransitiveStore =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "no-transitive-store"
        , ruleProse = "Components must not transitively import store modules."
        , ruleFix = "Remove the transitive store import."
        }

domainPurityTransitive :: BrokenRule
domainPurityTransitive =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "domain-purity-transitive"
        , ruleProse = "Domain must not transitively reach non-domain/shared modules."
        , ruleFix = "Keep domain pure."
        }

dirIsolation :: BrokenRule
dirIsolation =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "dir-isolation"
        , ruleProse = "Feature modules may only import from their own directory."
        , ruleFix = "Keep imports within the same directory or extract to shared."
        }

dirIsolationShared :: BrokenRule
dirIsolationShared =
    BrokenRule
        { rulebookId = RulebookId "test-rulebook"
        , ruleId = RuleId "dir-isolation-shared"
        , ruleProse = "Feature modules may only import from their own directory or shared."
        , ruleFix = "Keep imports within the same directory or use shared modules."
        }
