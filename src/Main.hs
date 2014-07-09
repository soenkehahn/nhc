
module Main where


import System.Environment

import Run


main :: IO ()
main = run =<< getArgs
