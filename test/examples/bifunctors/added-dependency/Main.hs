
module Main where


import           Control.Lens
import           Data.Bifunctor


main :: IO ()
main = do
  print $ bimap succ pred (1, 1)
