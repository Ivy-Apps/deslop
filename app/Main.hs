module Main (main) where

import Deslop (someFunc)

magic :: Int
magic = 42 * a
 where
  a :: Int
  a = 3

main :: IO ()
main = do
  putStrLn . show $ magic
  putStrLn "Hello, Haskell!"
  someFunc
