module Deslop.CasingSpec (spec) where

import Deslop.Casing
import Test.Hspec

spec :: Spec
spec = describe "Deslop.Casing" $ do
    decodeSpec
    spellsSpec
    renderingsSpec
    agreeSpec

--------------------------------------------------------------------------------
-- decode
--------------------------------------------------------------------------------

{- | Reading a spelling back. Exact for the two casings that carry a separator,
a guess for the two that mark boundaries with a capital.
-}
decodeSpec :: Spec
decodeSpec = describe "decode" $ do
    it "is exact for the casings that carry a separator" $ do
        decode KebabCase "http-client" `shouldBe` ["http", "client"]
        decode ConstantCase "HTTP_CLIENT" `shouldBe` ["http", "client"]
        decode ConstantCase "HTTP2_CLIENT" `shouldBe` ["http2", "client"]

    it "reads a run of capitals as one word" $ do
        decode PascalCase "HTTPClient" `shouldBe` ["http", "client"]
        decode PascalCase "DBConnection" `shouldBe` ["db", "connection"]
        decode PascalCase "IOStream" `shouldBe` ["io", "stream"]
        decode CamelCase "httpClient" `shouldBe` ["http", "client"]

    it "leaves a single-cased name alone" $ do
        decode PascalCase "UserProfile" `shouldBe` ["user", "profile"]
        decode PascalCase "Http2Client" `shouldBe` ["http2", "client"]

    it "cannot split two adjacent acronym words - a documented limitation" $ do
        decode PascalCase "AWSS3Client" `shouldBe` ["awss3", "client"]
        decode PascalCase "ABTest" `shouldBe` ["ab", "test"]

    it "cannot see a boundary that no capital marks - a documented limitation" $
        -- Capitalising "2fa" changes nothing, so Api2fa keeps its secret.
        decode PascalCase "Api2fa" `shouldBe` ["api2fa"]

    it "ignores empty segments a capture regex could still produce" $ do
        decode KebabCase "a--b" `shouldBe` ["a", "b"]
        decode KebabCase "-a-" `shouldBe` ["a"]

    it "handles names of three or more words" $ do
        decode KebabCase "use-case-name" `shouldBe` ["use", "case", "name"]
        decode PascalCase "UseCaseName" `shouldBe` ["use", "case", "name"]
        decode CamelCase "useCaseName" `shouldBe` ["use", "case", "name"]
        decode ConstantCase "USE_CASE_NAME" `shouldBe` ["use", "case", "name"]
        decode PascalCase "UserProfileSettingsPageTitle"
            `shouldBe` ["user", "profile", "settings", "page", "title"]

    it "reads an acronym in the middle of a long name as one word" $ do
        decode PascalCase "ArchiveDBOrder" `shouldBe` ["archive", "db", "order"]
        decode PascalCase "HTTPClientPoolFactory" `shouldBe` ["http", "client", "pool", "factory"]
        decode CamelCase "archiveDBOrder" `shouldBe` ["archive", "db", "order"]

--------------------------------------------------------------------------------
-- spells
--------------------------------------------------------------------------------

{- | The exact question, and the one agreement is decided by: could this name,
written in this casing, have produced this text?
-}
spellsSpec :: Spec
spellsSpec = describe "spells" $ do
    it "accepts a word written as an acronym" $ do
        spells PascalCase ["http", "client"] "HTTPClient" `shouldBe` True
        spells PascalCase ["http", "client"] "HttpClient" `shouldBe` True
        spells PascalCase ["aws", "s3"] "AWSS3" `shouldBe` True
        spells CamelCase ["http", "client"] "httpCLIENT" `shouldBe` True

    it "accepts a boundary that decode cannot recover" $ do
        spells PascalCase ["api", "2fa"] "Api2fa" `shouldBe` True
        spells PascalCase ["a", "b"] "AB" `shouldBe` True

    it "rejects a spelling of a different name" $ do
        spells PascalCase ["http", "client"] "HttpCache" `shouldBe` False
        spells PascalCase ["http", "client"] "Httpclient" `shouldBe` False
        spells KebabCase ["http", "client"] "http_client" `shouldBe` False

    it "leaves the separator-bearing casings with exactly one spelling" $ do
        spells KebabCase ["http", "client"] "http-client" `shouldBe` True
        spells KebabCase ["http", "client"] "HTTP-CLIENT" `shouldBe` False
        spells ConstantCase ["http", "client"] "HTTP_CLIENT" `shouldBe` True

--------------------------------------------------------------------------------
-- renderings
--------------------------------------------------------------------------------

renderingsSpec :: Spec
renderingsSpec = describe "renderings" $ do
    it "gives a separator-bearing casing exactly one spelling" $ do
        toList (renderings KebabCase ["db", "connection"]) `shouldBe` ["db-connection"]
        toList (renderings ConstantCase ["db", "connection"]) `shouldBe` ["DB_CONNECTION"]

    it "gives every acronym spelling for the casings that mark with a capital" $
        toList (renderings PascalCase ["db", "connection"])
            `shouldMatchList` ["DbConnection", "DbCONNECTION", "DBConnection", "DBCONNECTION"]

    it "puts the canonical spelling first" $
        head (renderings PascalCase ["db", "connection"]) `shouldBe` "DbConnection"

    it "does not repeat a word whose two spellings coincide" $
        toList (renderings PascalCase ["a", "b"]) `shouldBe` ["AB"]

    it "keeps a camel spelling's first word lower-case" $
        toList (renderings CamelCase ["db", "connection"])
            `shouldMatchList` ["dbConnection", "dbCONNECTION"]

--------------------------------------------------------------------------------
-- agree
--------------------------------------------------------------------------------

agreeSpec :: Spec
agreeSpec = describe "agree" $ do
    let nameOf = fmap (.canonical) . agree

    it "settles on the name an exact occurrence pins down" $ do
        nameOf ((KebabCase, "http-client") :| [(PascalCase, "HTTPClient")])
            `shouldBe` Just ["http", "client"]
        nameOf ((KebabCase, "aws-s3") :| [(PascalCase, "AWSS3")])
            `shouldBe` Just ["aws", "s3"]
        nameOf ((KebabCase, "api-2fa") :| [(PascalCase, "Api2fa")])
            `shouldBe` Just ["api", "2fa"]
        nameOf ((ConstantCase, "HTTP2_CLIENT") :| [(PascalCase, "Http2Client")])
            `shouldBe` Just ["http2", "client"]

    it "falls back to the coarsest reading when nothing pins it down" $ do
        nameOf (one (PascalCase, "DBConnection")) `shouldBe` Just ["db", "connection"]
        nameOf (one (PascalCase, "AWSS3")) `shouldBe` Just ["awss3"]

    it "refuses when no single name spells every occurrence" $ do
        nameOf ((KebabCase, "stripe-connect") :| [(PascalCase, "Paypal")]) `shouldBe` Nothing
        nameOf ((KebabCase, "http-client") :| [(PascalCase, "HttpCache")]) `shouldBe` Nothing

    it "keeps every reading an occurrence could have had" $
        -- ABTest is 'ab test' or 'a b test'; a forbidding clause widens over both.
        fmap (toList . (.candidates)) (agree (one (PascalCase, "ABTest")))
            `shouldBe` Just [["ab", "test"], ["a", "b", "test"]]

    it "keeps one reading when an exact occurrence rules the others out" $
        fmap (toList . (.candidates)) (agree ((KebabCase, "ab-test") :| [(PascalCase, "ABTest")]))
            `shouldBe` Just [["ab", "test"]]

    it "settles a name of three or more words" $ do
        nameOf ((KebabCase, "use-case-name") :| [(PascalCase, "UseCaseName")])
            `shouldBe` Just ["use", "case", "name"]
        nameOf ((KebabCase, "archive-db-order") :| [(PascalCase, "ArchiveDBOrder")])
            `shouldBe` Just ["archive", "db", "order"]
        nameOf ((ConstantCase, "HTTP_CLIENT_POOL") :| [(CamelCase, "httpClientPool")])
            `shouldBe` Just ["http", "client", "pool"]

    it "refuses a long name whose occurrences differ in one word" $
        nameOf ((KebabCase, "archive-db-order") :| [(PascalCase, "ArchiveDBItem")])
            `shouldBe` Nothing
