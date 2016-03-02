module Main where

import Lib

main :: IO ()
main = do
  s <- getLine
  putStrLn (someFunc s)
