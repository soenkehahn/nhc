
module Main where


import Data.Bifunctor


main :: IO ()
main = do
  print $ bimap succ pred (1, 1)
