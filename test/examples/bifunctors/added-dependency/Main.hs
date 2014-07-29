
module Main where


import           Safe
import           Data.Bifunctor


main :: IO ()
main = do
  print $ bimap succ pred (1, 1)
