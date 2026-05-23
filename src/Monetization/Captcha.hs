module Monetization.Captcha (
    Captcha (..),
    additionCaptcha,
    subtractionCaptcha,
    randomCaptcha,
) where

import Data.List ((!!))
import Effectful
import Effects.Random (Random, rGenRandomInt)

data Captcha = Captcha
    { challenge :: Text
    , answer :: Text
    }
    deriving stock (Show, Eq)

randomCaptcha :: (Random :> es) => Eff es Captcha
randomCaptcha = do
    let generators = [additionCaptcha, subtractionCaptcha]
    idx <- rGenRandomInt (0, length generators - 1)
    generators !! idx

additionCaptcha :: (Random :> es) => Eff es Captcha
additionCaptcha = do
    a <- rGenRandomInt (0, 99)
    b <- rGenRandomInt (0, (100 - a))
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
