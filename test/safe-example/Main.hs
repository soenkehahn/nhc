
module Main where


import System.Environment
import Safe


main :: IO ()
main = do
  args <- getArgs
  print $ headMay args
