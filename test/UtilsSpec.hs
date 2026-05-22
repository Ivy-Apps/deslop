module UtilsSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Utils (firstJustM)

spec :: Spec
spec = describe "Utils" $ do
    describe "firstJustM" $ do
        it "returns Nothing for an empty list" $ do
            firstJustM (\_ -> pure $ Just ()) [] `shouldReturn` Nothing

        it "returns the first Just result" $ do
            firstJustM (\x -> pure $ if x == 'b' then Just x else Nothing) ['a', 'b', 'c']
                `shouldReturn` Just 'b'

        it "returns Nothing when all actions return Nothing" $ do
            firstJustM alwaysNothing ['a', 'b', 'c']
                `shouldReturn` Nothing

        it "stops at the first Just and does not evaluate further" $ do
            ref <- newIORef (0 :: Int)
            let action x = modifyIORef ref (+ 1) >> pure (if x == 'a' then Just x else Nothing)
            result <- firstJustM action ['a', 'b', 'c']
            result `shouldBe` Just 'a'
            readIORef ref `shouldReturn` 1

        it "returns Just for a singleton list with a matching action" $ do
            firstJustM (pure . Just) [42 :: Int]
                `shouldReturn` Just 42

        it "returns Nothing for a singleton list with a non-matching action" $ do
            firstJustM alwaysNothing [42 :: Int]
                `shouldReturn` Nothing

alwaysNothing :: (Monad m) => a -> m (Maybe Int)
alwaysNothing _ = pure Nothing
