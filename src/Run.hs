{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Run where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Data.String.Interpolate
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.Files
import           System.Process
import           System.SetEnv

import           NhcOptions
import           Utils


nhcDir :: FilePath
nhcDir = ".nhc"

-- | Allows to use 'exitWith' inside the given action and
-- have the used 'ExitCode' being returned and not thrown.
handleExitCodes :: IO ExitCode -> IO ExitCode
handleExitCodes = handle $ \ (exitCode :: ExitCode) -> return exitCode

run :: [String] -> (Handle, Handle) -> IO ExitCode
run args handles =
  handleExitCodes $
  withNhcOptions args $ \ nhcOptions (command : args) -> do
    -- prerequisites
    createDirectoryIfMissing True nhcDir
    cabalFile <- getCabalFile
    defaultFile <- createDefaultNixFileIfMissing nhcOptions (takeBaseName cabalFile)
    nhcFile <- createNhcNixFileIfMissing nhcOptions defaultFile
    -- building the environment
    resultLink <- nixBuild cabalFile nhcFile
    -- creating our own environment script
    envSetup <- createEnvSetup resultLink
    -- entering the environment and performing the given command
    performCommand cabalFile resultLink envSetup command args handles


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
    let file = fromMaybe (nhcDir </> "default.nix") (customDefaultFile options)
    exists <- fileExist file
    when (not exists) $
        writeFile file $ normalizeLines [i|
            { pkgs ? import <nixpkgs> {},
              src ? builtins.filterSource (path: type:
                type != "unknown" &&
                baseNameOf path != ".git" &&
                baseNameOf path != "result" &&
                baseNameOf path != "dist" &&
                baseNameOf path != ".nhc") ../.
            }:
            {
                build = pkgs.haskellPackages.buildLocalCabal src "#{packageName}";
            }
          |]
    return file

-- | Creates a file 'nhc.nix' that is used to build the environment for
-- checking the haskell sources. If the file already exists it is left
-- untouched. This allows for modifying the build environment, e.g. for
-- profiling.
createNhcNixFileIfMissing :: NhcOptions -> FilePath -> IO FilePath
createNhcNixFileIfMissing options defaultFile = do
    let file = nhcDir </> "nhc.nix"
    exists <- doesFileExist file
    hadProfiling <- doesFileExist (nhcDir </> "prof")
    when (profiling options) $
      writeFile (nhcDir </> "prof") ""
    when (not exists || hadProfiling /= profiling options) $ do
        writeFile file $ normalizeLines [i|

            let

                pkgs = import <nixpkgs> {
                    config.allowUnfree = true;
                    config.cabal.libraryProfiling = #{if profiling options then "true" else "false"};
                };

                # https://github.com/schell/hdevtools/commit/9e34f7dd20fcf3654a57fbf414be4962cc279854
                git_hdevtools_src = pkgs.fetchgit {
                    url = "https://github.com/schell/hdevtools.git";
                    rev = "9e34f7dd20fcf3654a57fbf414be4962cc279854";
                    sha256 = "1720953a6a2dfd1bc1f2c0d1346834e44a452acfcaa6d8226216178fecb95de9";
                };

                hsEnv = pkgs.haskellPackages.ghcWithPackages
                    (hsPkgs :
                     let package = (hsPkgs.callPackage #{".." </> defaultFile} { inherit pkgs; }).build;
                     in
                        [ (hsPkgs.buildLocalCabal git_hdevtools_src "hdevtools") ] ++
                        package.buildInputs ++
                        package.nativeBuildInputs ++
                        package.propagatedBuildInputs ++
                        package.propagatedNativeBuildInputs ++
                        package.extraBuildInputs);

            in

                pkgs.myEnvFun {
                    name = "nhc-build";
                    buildInputs = [ hsEnv ];
                    extraCmds = ''
                        $(grep export ${hsEnv.outPath}/bin/ghc)
                    '';
                }
            |]
    return file

-- | Creates a symlink 'result' pointing to a script in the nix store
-- that sets up an environment for building the cabal package.
-- Omits any action when 'result' already exists and the cabal file is
-- not newer than 'result'.
nixBuild :: FilePath -> FilePath -> IO FilePath
nixBuild cabalFile nhcFile = do
    resultExists <- fileExist link
    if not resultExists then run else do
        cabalModTime <- modificationTime <$> getSymbolicLinkStatus cabalFile
        nhcModTime <- modificationTime <$> getSymbolicLinkStatus nhcFile
        resultModTime <- modificationTime <$> getSymbolicLinkStatus link
        when (cabalModTime >= resultModTime ||
              nhcModTime >= resultModTime)
            run
    return link
  where
    link = nhcDir </> "result"

    run :: IO ()
    run = do
        hPutStrLn stderr "building..."
        exitCode <- system [i|nix-build #{nhcFile} -j4 -o #{link}|]
        when (exitCode /= ExitSuccess) $ do
          hPutStrLn stderr "error executing nix-build to build environment"
          exitWith exitCode
        return ()

createEnvSetup :: FilePath -> IO FilePath
createEnvSetup buildResult = do
  let file = nhcDir </> "nhc-env.sh"
  contents <- readFile (buildResult </> "bin/load-env-nhc-build")
  writeFile file (fiddleInArgument contents)
  ExitSuccess <- system [i|chmod +x #{file}|]
  return file
 where
  fiddleInArgument :: String -> String
  fiddleInArgument script = unlines $
      unlines (take 13 (lines script)) :
      []


-- | Performs the command inside the environment.
performCommand :: FilePath -> FilePath -> FilePath -> String -> [String] -> (Handle, Handle) -> IO ExitCode
performCommand cabalFile resultLink envSetup command args (stdin, stdout) = do
    envSetupContents <- readFile envSetup
    hdevStopLine <- stopHdevtoolsIfNecessary resultLink
    writeFile (nhcDir </> "tmp_command.sh") $ unlines $
      envSetupContents :
      fromMaybe "" hdevStopLine :
      unwords (command : map wrapBashArg args) :
      []
    getPermissions (nhcDir </> "tmp_command.sh") >>= \ p ->
      setPermissions (nhcDir </> "tmp_command.sh") (setOwnerExecutable True p)
    unsetEnv "_PATH"
    setEnv "NHC_CABAL_FILE" =<< canonicalizePath cabalFile
    (Nothing, Nothing, Nothing, process) <- createProcess $ (proc (nhcDir </> "tmp_command.sh") []) {
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
