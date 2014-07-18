
module Main where


import           System.Environment
import           System.Exit
import           System.IO

import           Run


main :: IO ()
main = do
  args <- getArgs
  run args (stdin, stdout) >>= exitWith
