{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Run where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Data.String.Interpolate
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.Files
import           System.Process

import           NhcOptions
import           Utils


nhcDir :: FilePath
nhcDir = ".nhc"

-- | Allows to use 'exitWith' inside the given action and
-- have the used 'ExitCode' being returned and not thrown.
handleExitCodes :: IO ExitCode -> IO ExitCode
handleExitCodes = handle $ \ (exitCode :: ExitCode) -> return exitCode

run :: (Handle, Handle) -> [String] -> IO ExitCode
run handles args =
  handleExitCodes $
  withNhcOptions args $ \ nhcOptions command ->
    if clean nhcOptions then do
      exists <- doesDirectoryExist nhcDir
      when exists $
        removeDirectoryRecursive nhcDir
      return ExitSuccess
     else do
      case command of
        [] -> do
          progName <- getProgName
          hPutStr stderr $ normalizeLines [i|
            No command provided.
            Try '#{progName} --help'.
           |]
          return $ ExitFailure 1
        (command : args) -> execute handles nhcOptions command args

execute :: (Handle, Handle) -> NhcOptions -> String -> [String] -> IO ExitCode
execute handles nhcOptions command args = do
  -- prerequisites
  createDirectoryIfMissing True nhcDir
  cabalFile <- getCabalFile
  defaultFile <- createDefaultNixFileIfMissing nhcOptions (takeBaseName cabalFile)
  -- entering the environment and performing the given command
  performCommand cabalFile defaultFile command args handles


getCabalFile :: IO FilePath
getCabalFile = do
    fs <-
        filter (not . null . takeBaseName) <$>
        filter ((== ".cabal") . takeExtension) <$>
        getDirectoryContents "."
    case fs of
        [f] -> return f
        [] -> throwIO $ ErrorCall "no cabal file found"
        fs -> throwIO $ ErrorCall ("found multiple cabal files: " ++ unwords fs)

createDefaultNixFileIfMissing :: NhcOptions -> String -> IO FilePath
createDefaultNixFileIfMissing options packageName = do
    file <- lookupDefaultFile options
    exists <- fileExist file
    when (not exists) $
        writeFile file $ normalizeLines [i|
            { pkgs ? import <nixpkgs> {},
              zalora-nix-lib ? import <zalora-nix-lib> { inherit pkgs; },
              src ? builtins.filterSource (path: type:
                type != "unknown" &&
                baseNameOf path != ".git" &&
                baseNameOf path != "result" &&
                baseNameOf path != "dist" &&
                baseNameOf path != ".nhc") ../.
            }:
            zalora-nix-lib.haskell.buildHaskell {
              name = "#{packageName}";
              inherit src;
            }
          |]
    return file

lookupDefaultFile :: NhcOptions -> IO FilePath
lookupDefaultFile options = do
    defaultFromWorkingDirExists <- doesFileExist "./default.nix"
    let defaultFromWorkingDir = if defaultFromWorkingDirExists
          then Just "./default.nix"
          else Nothing
    return $ fromMaybe (nhcDir </> "default.nix") $
        customDefaultFile options <|>
        defaultFromWorkingDir


-- | Performs the command inside the environment.
performCommand :: FilePath -> FilePath -> String -> [String] -> (Handle, Handle) -> IO ExitCode
performCommand cabalFile expressionFile command args (stdin, stdout) = do
    writeFile (nhcDir </> "script.sh") (unwords (command : map wrapBashArg args) ++ "\n")
    unsetEnv "_PATH"
    let nixShellArgs =
          "--command" : unwords (("NHC_CABAL_FILE=" ++ cabalFile) : "bash" : (nhcDir </> "script.sh") : []) :
          "--max-jobs" : "4" : -- number of cpus?
          expressionFile :
          []
    (Nothing, Nothing, Nothing, process) <- createProcess
        (proc "nix-shell" nixShellArgs) {
            std_in = UseHandle stdin,
            std_out = UseHandle stdout
            -- delegate_ctlc = True -- only in process 1.2.0.0 :(
        }
    waitForProcess process

wrapBashArg :: String -> String
wrapBashArg a =
  "\"" ++ a ++ "\""


-- | hdevtools starts a background daemon in the environment it is first
-- invocated in. If 'result' is newer than the hdevtools socket, we have
-- to shut down the daemon. This will cause a new startup in the new
-- environment.
stopHdevtoolsIfNecessary :: FilePath -> IO (Maybe String)
stopHdevtoolsIfNecessary resultLink = do
    socketExists <- fileExist ".hdevtools.sock"
    if socketExists then do
        hdevSocketModTime <- modificationTime <$> getSymbolicLinkStatus ".hdevtools.sock"
        resultModTime <- modificationTime <$> getSymbolicLinkStatus resultLink
        return $ if resultModTime >= hdevSocketModTime
          then Just "hdevtools --stop-server"
          else Nothing
    else
        return Nothing
