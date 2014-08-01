{-# LANGUAGE QuasiQuotes #-}

module NhcOptions where


import           Control.Applicative
import           Data.List
import           Data.String.Interpolate
import           Options
import           System.Environment
import           System.Exit
import           System.IO

import           Utils


data NhcOptions = NhcOptions {
    profiling :: Bool,
    clean :: Bool,
    customDefaultFile :: Maybe FilePath
  }
    deriving Show

instance Options NhcOptions where
  defineOptions = NhcOptions <$>
    defineOption optionType_bool (\ o -> o{
        optionLongFlags = ["prof"],
        optionShortFlags = ['p'],
        optionDescription =
          "Creates an environment where the profiling versions \
          \of all dependencies are available."
      }) <*>
    defineOption optionType_bool (\ o -> o{
        optionLongFlags = ["clean"],
        optionShortFlags = ['c'],
        optionDescription =
          "If this flag is set, nhc will clean all files it created. \
          \(And do nothing else.)"
      }) <*>
    simpleOption "custom-default" Nothing
      "Custom default.nix file to be used. \
      \(Has to be used with an equal sign, \
      \i.e '--custom-default=custom.nix'.)"

withNhcOptions :: [String] -> (NhcOptions -> [String] -> IO ExitCode) -> IO ExitCode
withNhcOptions args action = do
  let (nhcArgs, command) = span ("-" `isPrefixOf`) args
      parsed = parseOptions nhcArgs
  case parsedOptions parsed of
    Just opts ->  action opts command
    Nothing -> case parsedError parsed of
      Nothing -> do
        progName <- getProgName
        putStrLn $ normalizeLines [i|
          #{progName} - execute commands inside a haskell build environment

          Allows you to easily execute arbitrary commands inside a build
          environment that is set up (through nix: http://nixos.org/)
          according to the cabal file in the current dirctory. With each
          invocation, the cabal file is checked for modifications and the
          build environment is updated accordingly.
          https://github.com/soenkehahn/nhc

          Usage:
            #{progName} [OPTIONS] COMMAND
            All OPTIONS start with one or two dashes ('-'). The first argument
            that doesn't start with a dash and all following arguments are 
            interpreted as the COMMAND that will be executed in the build
            environment. (Shell expansion is of course done outside the build
            environment by the outer shell.)
          |]
        putStrLn $ normalizeLines $ parsedHelp parsed
        putStr $ normalizeLines [i|
          Examples:
            #{progName} cabal build
            #{progName} hdevtools check src/Main.hs
            #{progName} ghci
            #{progName} bash
          |]
        return $ ExitSuccess
      Just errorMessage -> do
        hPutStrLn stderr errorMessage
        return $ ExitFailure 1
