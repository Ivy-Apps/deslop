module FileSystem.PathSpec (spec) where

import Data.Text qualified as T
import FileSystem.Path (
    AbsPath (..),
    ProjectRoot (..),
    RelativePath (..),
    decodeOsPath,
    relativePathTo,
 )
import Hedgehog (Gen, assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath (isRelative)
import Test.Hspec
import TestUtils (ap, prop, rp)

{- | A path spelled the way a golden or a Problem Id spells it, so a failure
reads as the string a user would see rather than as an 'OsPath'.
-}
shown :: RelativePath -> Text
shown = decodeOsPath . (.osPath)

root :: Text -> ProjectRoot
root = ProjectRoot . ap

spec :: Spec
spec = describe "FileSystem.Path" $ do
    describe "relativePathTo" $ do
        it "strips the root from a file inside it" $ do
            relativePathTo (root "/repo") (ap "/repo/src/a.ts") `shouldBe` rp "src/a.ts"

        it "is the file name for a file directly in the root" $ do
            relativePathTo (root "/repo") (ap "/repo/a.ts") `shouldBe` rp "a.ts"

        it "goes back out of the root for a sibling directory" $ do
            relativePathTo (root "/repo/packages/app") (ap "/repo/shared/base.json")
                `shouldBe` rp "../../shared/base.json"

        it "goes back out of the root for a parent-directory file" $ do
            relativePathTo (root "/repo/packages/app") (ap "/repo/tsconfig.json")
                `shouldBe` rp "../../tsconfig.json"

        it "is '.' for the root itself" $ do
            relativePathTo (root "/repo") (ap "/repo") `shouldBe` rp "."

        it "never returns the target verbatim when the root is not a prefix" $ do
            -- makeRelative's behaviour, and the reason a Problem Id could hold
            -- an absolute path: the answer has to stay relative either way.
            shown (relativePathTo (root "/repo/config") (ap "/repo/packages/app/src/main.ts"))
                `shouldBe` "../packages/app/src/main.ts"

        prop "is always relative, whatever the root" $ do
            base <- forAll genAbsPath
            target <- forAll genAbsPath

            let result = relativePathTo (ProjectRoot base) target

            assert . isRelative $ result.osPath

        prop "round-trips a path under the root" $ do
            base <- forAll (genSegments "b" 1 4)
            rest <- forAll (genSegments "r" 1 4)

            let result = relativePathTo (root (joinAbs base)) (ap (joinAbs (base <> rest)))

            shown result === T.intercalate "/" rest

        prop "goes back once per segment the root does not share" $ do
            shared <- forAll (genSegments "s" 1 3)
            onlyRoot <- forAll (genSegments "r" 0 3)
            onlyTarget <- forAll (genSegments "t" 1 3)

            let result =
                    relativePathTo
                        (root . joinAbs $ shared <> onlyRoot)
                        (ap . joinAbs $ shared <> onlyTarget)

            shown result
                === T.intercalate "/" (replicate (length onlyRoot) ".." <> onlyTarget)

joinAbs :: [Text] -> Text
joinAbs = ("/" <>) . T.intercalate "/"

{- | Segments tagged by which part of the path they belong to and by their
position, so that a generated root and target share exactly the prefix the
test says they share, never one more by coincidence.
-}
genSegments :: Text -> Int -> Int -> Gen [Text]
genSegments tag lo hi = do
    n <- Gen.int (Range.linear lo hi)
    pure [tag <> show i | i <- [1 .. n]]

{- | Any absolute path at all: two draws may share a prefix, share nothing, or
be equal, which is the point when the claim is that the answer is relative
whatever the two paths are.
-}
genAbsPath :: Gen AbsPath
genAbsPath = do
    n <- Gen.int (Range.linear 0 4)
    ap . joinAbs <$> Gen.list (Range.singleton n) (Gen.element ["a", "b", "c"])
