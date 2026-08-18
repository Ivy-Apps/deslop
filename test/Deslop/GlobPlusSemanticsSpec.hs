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

    describe "G. .. goes one directory back, in a clause only" $ do
        {- The rule the four cases below are read against. The target is
        depth-pinned, so {{TARGET_DIR}} is the feature folder for every file it
        matches and `..` therefore means the same thing every time. -}
        let featureTarget = "@/client/{{feature-name}}/{{FileName}}View"
        let allows = clauseMatches featureTarget "@/client/home/HomeView"

        it "reaches a sibling of the matched file's own directory" $ do
            allows "{{TARGET_DIR}}/../shared/**" "@/client/shared/Button" `shouldBe` True
            allows "{{TARGET_DIR}}/../shared/**" "@/client/shared/forms/Input" `shouldBe` True
            allows "{{TARGET_DIR}}/../shared/**" "@/client/home/shared/Button" `shouldBe` False
            allows "{{TARGET_DIR}}/../shared/**" "@/client/billing/shared/Button" `shouldBe` False

        it "goes back exactly one directory per .., never the whole substitution" $ do
            allows "{{TARGET_DIR}}/../../shared/**" "@/shared/Button" `shouldBe` True
            allows "{{TARGET_DIR}}/../../shared/**" "@/client/shared/Button" `shouldBe` False

        it "does nothing when there is nothing left to go back past" $ do
            allows "{{TARGET_DIR}}/../../../shared/**" "shared/Button" `shouldBe` True
            allows "../shared/**" "shared/Button" `shouldBe` True

        it "means a different directory for a file at a different depth" $ do
            let deep = clauseMatches "@/client/{{feature-name}}/**/{{FileName}}View" "@/client/home/widgets/CardView"
            deep "{{TARGET_DIR}}/../shared/**" "@/client/home/shared/Icon" `shouldBe` True
            deep "{{TARGET_DIR}}/../shared/**" "@/client/shared/Button" `shouldBe` False

    describe ".. may only go back past a segment the pattern determines" $ do
        it "rejects going back past a **, which is zero or many segments" $
            clauseError "@/client/**/../shared" `shouldBe` Just (ParentDirPastWildcard "**")

        it "rejects going back past a bare *, which names no directory" $
            clauseError "@/client/*/../shared" `shouldBe` Just (ParentDirPastWildcard "*")

        it "rejects going back past a segment that merely contains a *" $
            clauseError "@/client/*View/../shared" `shouldBe` Just (ParentDirPastWildcard "*View")

        it "checks each .. of a chain against what it would actually reach" $ do
            clauseError "@/a*/b/../shared" `shouldBe` Nothing
            clauseError "@/a*/b/../../shared" `shouldBe` Just (ParentDirPastWildcard "a*")

        it "accepts going back past a literal, a variable or TARGET_DIR" $ do
            clauseError "@/client/home/../shared" `shouldBe` Nothing
            clauseError "@/client/{{provider-name}}/../shared" `shouldBe` Nothing
            clauseError "{{TARGET_DIR}}/../shared" `shouldBe` Nothing

        it "leaves a ** alone when the .. goes back past something else" $
            clauseError "@/client/**/widgets/../shared" `shouldBe` Nothing

    describe ".. belongs to a clause, which is the only pattern with a directory" $ do
        it "rejects .. in a target pattern" $
            compileError "@/client/../shared/**" `shouldBe` Just ParentDirInTargetPattern

        it "rejects .. in an exclude pattern" $
            excludeError "@/client/../shared/**" `shouldBe` Just ParentDirInExcludePattern

        it "reads a dotted segment that is not exactly .. as ordinary text" $ do
            compileError "@/client/..shared/x" `shouldBe` Nothing
            compileError "@/client/.../x" `shouldBe` Nothing
            compileError "@/client/a..b/x" `shouldBe` Nothing

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

excludeError :: Text -> Maybe GlobPlusError
excludeError = leftToMaybe . compileExcludePattern

{- | Whether a clause matches a path, under the environment a target pattern
binds for one file. Written in terms of the two patterns and the two paths so a
case reads as the rule an author would have written.
-}
clauseMatches :: Text -> Text -> Text -> Text -> Bool
clauseMatches targetPattern targetPath clause candidate =
    matchClause (compiledClause compiledTarget.boundVars) env (segmentsOf candidate)
  where
    compiledTarget = case compileTargetPattern targetPattern of
        Right compiled -> compiled
        Left err -> error $ "target pattern did not compile: " <> renderGlobPlusError err
    compiledClause bound = case compileClausePattern Narrow bound clause of
        Right compiled -> compiled
        Left err -> error $ "clause pattern did not compile: " <> renderGlobPlusError err
    env = case matchTarget compiledTarget (segmentsOf targetPath) of
        Just matched' -> matched'
        Nothing -> error $ targetPattern <> " did not match " <> targetPath

{- | Compiles a clause in a scope binding every variable these cases name, so
that a failure is about the pattern's shape rather than about its scope.
-}
clauseError :: Text -> Maybe GlobPlusError
clauseError =
    leftToMaybe
        . compileClausePattern Narrow (fromList [VarName "provider-name", VarName "service-type", VarName "file-name"])

kebabOf :: Text -> MatchEnv -> Maybe Text
kebabOf name env = casedAs KebabCase <$> Map.lookup (VarName name) env.variables
