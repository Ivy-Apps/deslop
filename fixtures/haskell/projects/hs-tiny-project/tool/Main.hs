module Main (main) where

import Tiny.Core qualified as Core
import Data.Text.IO qualified as T

main :: IO ()
main = T.putStrLn (Core.greet (Core.Name "tool"))
