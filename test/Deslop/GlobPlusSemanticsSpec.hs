{- | The structural semantics of Glob+ matching, case by case.

Every case here comes from the matrix measured against @d34bf41@ and posted on
PR #184. The groups keep their letters from that matrix so a row can be looked
up: A is the shapes that already worked, B and C are the wrong bindings that
motivated the rework, D is separator ambiguity, E is the missing boundary a @*@
leaves behind, and F is the zero-width globstar.

Cases that the rework turns into /compile errors/ sit in 'describe' blocks of
their own, because "this pattern has no meaning" is a different claim from
"this pattern binds the wrong thing".
-}
module Deslop.GlobPlusSemanticsSpec (spec) where

import Data.Map.Strict qualified as Map
import Deslop.GlobPlus
import Deslop.GlobPlus.Compiler
import Test.Hspec
import TestUtils (requireJust)

spec :: Spec
spec = describe "Deslop.GlobPlus semantics" $ do
    describe "A. a variable pinned by a literal on at least one side" $ do
        it "binds the segment before the file, whatever the depth above it" $ do
            let pat = "@/components/**/{{provider-name}}/{{FileName}}View"
            for_
                [ "@/components/stripe-connect/CheckoutView"
                , "@/components/a/stripe-connect/CheckoutView"
                , "@/components/a/b/stripe-connect/CheckoutView"
                ]
                $ \path -> do
                    env <- matched pat path
                    kebabOf "provider-name" env `shouldBe` Just "stripe-connect"
                    kebabOf "file-name" env `shouldBe` Just "checkout"

        it "binds a repeated variable across a globstar that follows it" $ do
            env <- matched "@/components/{{provider-name}}/**/{{ProviderName}}View" "@/components/stripe-connect/a/b/StripeConnectView"
            kebabOf "provider-name" env `shouldBe` Just "stripe-connect"

        it "binds a repeated variable across a globstar that precedes it" $ do
            env <- matched "@/**/{{provider-name}}/{{ProviderName}}View" "@/a/b/stripe-connect/StripeConnectView"
            kebabOf "provider-name" env `shouldBe` Just "stripe-connect"

        it "binds the same segment at every depth: the pattern decides, not the path" $ do
            let pat = "@/{{provider-name}}/**/{{FileName}}View"
            for_
                [ "@/stripe-connect/CheckoutView"
                , "@/stripe-connect/payment/CheckoutView"
                , "@/stripe-connect/payment/gateway/CheckoutView"
                ]
                $ \path -> do
                    env <- matched pat path
                    kebabOf "provider-name" env `shouldBe` Just "stripe-connect"

    describe "B & C. a variable with ** on both sides has no meaning" $ do
        it "rejects a lone unanchored variable" $
            compileError "@/**/{{provider-name}}/**/{{FileName}}View"
                `shouldBe` Just (UnanchoredVariable (VarName "provider-name"))

        it "rejects an unanchored variable even when it is repeated" $
            compileError "@/**/{{provider-name}}/**/{{ProviderName}}Entry"
                `shouldBe` Just (UnanchoredVariable (VarName "provider-name"))

        it "rejects an unanchored variable with a trailing globstar" $
            compileError "@/**/{{provider-name}}/**"
                `shouldBe` Just (UnanchoredVariable (VarName "provider-name"))

        it "rejects the middle variable of three, naming that one" $
            compileError "@/{{provider-name}}/**/{{service-type}}/**/{{FileName}}View"
                `shouldBe` Just (UnanchoredVariable (VarName "service-type"))

        it "accepts a variable anchored from the start, and one from the end" $ do
            compileError "@/{{provider-name}}/**/{{FileName}}View" `shouldBe` Nothing
            compileError "@/**/{{provider-name}}/{{FileName}}View" `shouldBe` Nothing

        it "allows an unanchored variable in a clause, which substitutes rather than captures" $
            clauseError "@/**/{{provider-name}}/**" `shouldBe` Nothing

    describe "D. a separator both variables could consume" $ do
        it "binds greedy-left when nothing constrains the split" $ do
            env <- matched "@/c/{{provider-name}}-{{service-type}}" "@/c/stripe-connect-payment"
            kebabOf "provider-name" env `shouldBe` Just "stripe-connect"
            kebabOf "service-type" env `shouldBe` Just "payment"

        it "binds the only split there is" $ do
            env <- matched "@/c/{{provider-name}}-{{service-type}}" "@/c/stripe-payment"
            kebabOf "provider-name" env `shouldBe` Just "stripe"
            kebabOf "service-type" env `shouldBe` Just "payment"

        it "lets an earlier occurrence choose the split, not merely validate it" $ do
            -- The greedy split is provider-name=stripe-connect, which the first
            -- segment has already ruled out. The search must go on to the next.
            env <- matched "@/c/{{provider-name}}/{{provider-name}}-{{service-type}}" "@/c/stripe/stripe-connect-payment"
            kebabOf "provider-name" env `shouldBe` Just "stripe"
            kebabOf "service-type" env `shouldBe` Just "connect-payment"

        it "still rejects a path where no split agrees" $
            match "@/{{provider-name}}/{{provider-name}}-service" "@/paypal/stripe-service" `shouldBe` Nothing

        it "accepts a path where the repeated variable agrees exactly" $ do
            env <- matched "@/{{provider-name}}/{{provider-name}}-service" "@/stripe-connect/stripe-connect-service"
            kebabOf "provider-name" env `shouldBe` Just "stripe-connect"

    describe "E. two variables need a literal between them" $ do
        it "rejects directly adjacent variables" $
            compileError "@/x/{{FileName}}{{ServiceType}}"
                `shouldBe` Just (NoBoundaryBetween "FileName" "ServiceType")

        it "rejects variables separated only by *, which can match nothing" $
            compileError "@/x/{{FileName}}*{{ServiceType}}"
                `shouldBe` Just (NoBoundaryBetween "FileName" "ServiceType")

        it "accepts variables separated by a literal" $
            compileError "@/x/{{provider-name}}-{{service-type}}" `shouldBe` Nothing

        it "accepts variables in separate segments" $
            compileError "@/x/{{provider-name}}/{{service-type}}" `shouldBe` Nothing

    describe "F. ** stands for zero or many segments, everywhere" $ do
        it "matches the folder module itself, with the globstar standing for nothing" $ do
            env <- matched "@/components/{{provider-name}}/**" "@/components/stripe-connect"
            kebabOf "provider-name" env `shouldBe` Just "stripe-connect"

        it "matches below the folder too" $ do
            env <- matched "@/components/{{provider-name}}/**" "@/components/stripe-connect/a/b"
            kebabOf "provider-name" env `shouldBe` Just "stripe-connect"

        it "matches a bare prefix with a trailing globstar" $
            match "@/lib/**" "@/lib" `shouldNotBe` Nothing

        it "matches a leading globstar standing for nothing" $
            match "@/**/{{FileName}}View" "@/CheckoutView" `shouldNotBe` Nothing

        it "is idempotent: two globstars say what one says" $
            for_ ["@/a/b", "@/a/x/b", "@/a/x/y/b"] $ \path ->
                match "@/a/**/**/b" path `shouldBe` match "@/a/**/b" path

    describe "** is a whole segment or it is nothing" $ do
        it "rejects a globstar glued to a suffix" $
            compileError "@/a/**View" `shouldBe` Just (GlobStarNotWholeSegment "**View")

        it "rejects a globstar glued to a prefix" $
            compileError "@/a/View**" `shouldBe` Just (GlobStarNotWholeSegment "View**")

        it "accepts a globstar occupying its own segment" $
            compileError "@/a/**/View" `shouldBe` Nothing

    describe "* is a within-segment wildcard matching zero or more characters" $ do
        it "matches inside a segment, between literals" $
            match "@/features/**/use*ViewModel" "@/features/home/useHomeViewModel" `shouldNotBe` Nothing

        it "matches nothing at all" $
            match "@/a/use*ViewModel" "@/a/useViewModel" `shouldNotBe` Nothing

        it "never crosses a segment boundary" $
            match "@/features/*/page" "@/features/auth/login/page" `shouldBe` Nothing

        it "stands for exactly one segment when it is the whole segment" $ do
            match "@/features/*/page" "@/features/auth/page" `shouldNotBe` Nothing
            match "@/features/*/page" "@/features/page" `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

match :: Text -> Text -> Maybe MatchEnv
match pat path = case compileTargetPattern pat of
    Right compiled -> matchTarget compiled (segmentsOf path)
    Left err -> error $ "target pattern did not compile: " <> renderGlobPlusError err

matched :: Text -> Text -> IO MatchEnv
matched pat path = requireJust (toString (pat <> " did not match " <> path)) (match pat path)

compileError :: Text -> Maybe GlobPlusError
compileError = leftToMaybe . compileTargetPattern

{- | Compiles a clause in a scope binding every variable these cases name, so
that a failure is about the pattern's shape rather than about its scope.
-}
clauseError :: Text -> Maybe GlobPlusError
clauseError =
    leftToMaybe
        . compileClausePattern Narrow (fromList [VarName "provider-name", VarName "service-type", VarName "file-name"])

kebabOf :: Text -> MatchEnv -> Maybe Text
kebabOf name env = casedAs KebabCase <$> Map.lookup (VarName name) env.variables
