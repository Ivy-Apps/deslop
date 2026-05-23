module Monetization.Captcha (
    Captcha (..),
    additionCaptcha,
    subtractionCaptcha,
    randomCaptcha,
    triggerCaptcha,
) where

import Data.List ((!!))
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effects.CLI (CLI, cliReadLine, logText)
import Effects.Random (Random, rGenRandomInt)
import Types (DeslopError (CaptchaError))

data Captcha = Captcha
    { challenge :: Text
    , answer :: Text
    }
    deriving stock (Show, Eq)

triggerCaptcha ::
    ( Random :> es
    , CLI :> es
    , Error DeslopError :> es
    ) =>
    Eff es ()
triggerCaptcha = do
    captcha <- randomCaptcha
    logText $ "[CAPTCHA] Solve: " <> captcha.challenge
    logText "Type answer:"
    answer <- cliReadLine
    if answer /= captcha.answer
        then throwError CaptchaError
        else logText "Correct."

randomCaptcha :: (Random :> es) => Eff es Captcha
randomCaptcha = do
    let generators = [additionCaptcha, subtractionCaptcha]
    idx <- rGenRandomInt (0, length generators - 1)
    generators !! idx

additionCaptcha :: (Random :> es) => Eff es Captcha
additionCaptcha = do
    a <- rGenRandomInt (0, 99)
    b <- rGenRandomInt (0, 100 - a)
    pure
        Captcha
            { challenge = show a <> "+" <> show b <> "=?"
            , answer = show $ a + b
            }

subtractionCaptcha :: (Random :> es) => Eff es Captcha
subtractionCaptcha = do
    a <- rGenRandomInt (1, 100)
    -- ans = a - b; ans > 0 -> a > b
    b <- rGenRandomInt (0, a)
    pure
        Captcha
            { challenge = show a <> "-" <> show b <> "=?"
            , answer = show $ a - b
            }
