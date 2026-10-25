module Effects.BrowserSpec (spec) where

import Effects.Browser (Attendance (..), attendanceOf)
import Effects.CLI (Stdout (..))
import Test.Hspec

spec :: Spec
spec = describe "Effects.Browser" $ do
    describe "attendanceOf" $ do
        it "is attended on a terminal outside CI" $
            attendanceOf ToTerminal Nothing `shouldBe` Attended

        it "is unattended on a terminal in CI, whatever CI is set to" $
            forM_ ["true", "1", ""] $ \ci ->
                attendanceOf ToTerminal (Just ci) `shouldBe` Unattended

        it "is unattended when stdout is redirected, in CI or not" $ do
            attendanceOf Redirected Nothing `shouldBe` Unattended
            attendanceOf Redirected (Just "true") `shouldBe` Unattended
