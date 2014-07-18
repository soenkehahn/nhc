{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Run where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.List
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


run :: [String] -> (Handle, Handle) -> IO ExitCode
run args handles = withNhcOptions args $ \ NhcOptions command -> do
    -- prerequisites
    cabalFile <- getCabalFile
    defaultFile <- createDefaultNixFileIfMissing (takeBaseName cabalFile)
    nhcFile <- createNhcNixFileIfMissing defaultFile
    -- building the environment
    buildResult <- nixBuild cabalFile nhcFile
    -- creating our own environment script
    envScript <- createEnvScript buildResult
    -- entering the environment and performing the given command
    performCommand cabalFile envScript command handles


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

createDefaultNixFileIfMissing :: String -> IO FilePath
createDefaultNixFileIfMissing packageName = do
    exists <- fileExist "default.nix"
    when (not exists) $
        writeFile "default.nix" $ normalizeLines [i|
            { pkgs ? import <nixpkgs> {},
              src ? ./. } :
            {
                build = pkgs.haskellPackages.buildLocalCabal src "#{packageName}";
            }
          |]
    return "./default.nix"

-- | Creates a file 'nhc.nix' that is used to build the environment for
-- checking the haskell sources. If the file already exists it is left
-- untouched. This allows for modifying the build environment, e.g. for
-- profiling.
createNhcNixFileIfMissing :: FilePath -> IO FilePath
createNhcNixFileIfMissing defaultFile = do
    exists <- doesFileExist "nhc.nix"
    when (not exists) $ do
        writeFile "nhc.nix" $ normalizeLines [i|

            let

                pkgs = import <nixpkgs> { config.allowUnfree = true; };
                # this hack will be needed to get profiling to work:
                # pkgs = originalPkgs // {
                #     haskellPackages = originalPkgs.haskellPackages_ghc763;
                # };

                # https://github.com/schell/hdevtools/commit/9e34f7dd20fcf3654a57fbf414be4962cc279854
                git_hdevtools_src = pkgs.fetchgit {
                    url = "https://github.com/schell/hdevtools.git";
                    rev = "9e34f7dd20fcf3654a57fbf414be4962cc279854";
                    sha256 = "1720953a6a2dfd1bc1f2c0d1346834e44a452acfcaa6d8226216178fecb95de9";
                };

                hsEnv = pkgs.haskellPackages.ghcWithPackages
                    (hsPkgs :
                     let package = (hsPkgs.callPackage #{defaultFile} { inherit pkgs; }).build;
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
    return "nhc.nix"

-- | Creates a symlink 'result' pointing to a script in the nix store
-- that sets up an environment for building the cabal package.
-- Omits any action when 'result' already exists and the cabal file is
-- not newer than 'result'.
nixBuild :: FilePath -> FilePath -> IO FilePath
nixBuild cabalFile nhcFile = do
    resultExists <- fileExist "result"
    if not resultExists then run else do
        cabalModTime <- modificationTime <$> getSymbolicLinkStatus cabalFile
        nhcModTime <- modificationTime <$> getSymbolicLinkStatus nhcFile
        resultModTime <- modificationTime <$> getSymbolicLinkStatus "result"
        when (cabalModTime >= resultModTime ||
              nhcModTime >= resultModTime)
            run
    return "./result"
  where
    run :: IO ()
    run = do
        hPutStrLn stderr "building..."
        ExitSuccess <- system "nix-build nhc.nix -j4"
        return ()

createEnvScript :: FilePath -> IO FilePath
createEnvScript buildResult = do
  contents <- readFile (buildResult </> "bin/load-env-nhc-build")
  writeFile "nhc-env.sh" (fiddleInArgument contents)
  ExitSuccess <- system "chmod +x nhc-env.sh"
  return "./nhc-env.sh"
 where
  fiddleInArgument :: String -> String
  fiddleInArgument script = unlines $
      unlines (take 13 (lines script)) :
      "# hackishly generated by nhc" :
      unwords [bash, "--norc", "-c", "\"$1\""] :
      []
    where
      bash = case filter ("bash" `isInfixOf`) (words (unlines (drop 13 (lines script)))) of
        [x] -> x
        _ -> error ("format of bin/load/env-nhc-build changed")


-- | Performs the command inside the environment.
performCommand :: FilePath -> FilePath -> [String] -> (Handle, Handle) -> IO ExitCode
performCommand cabalFile envScript command (stdin, stdout) = do
    stopHdevtoolsIfNecessary
    unsetEnv "_PATH"
    setEnv "NHC_CABAL_FILE" =<< canonicalizePath cabalFile
    (Nothing, Nothing, Nothing, process) <- createProcess $ (proc envScript [unwords command]) {
      std_in = UseHandle stdin,
      std_out = UseHandle stdout
      -- delegate_ctlc = True -- only in process 1.2.0.0 :(
     }
    waitForProcess process

-- | hdevtools starts a background daemon in the environment it is first
-- invocated in. If 'result' is newer than the hdevtools socket, we have
-- to shut down the daemon. This will cause a new startup in the new
-- environment.
stopHdevtoolsIfNecessary :: IO ()
stopHdevtoolsIfNecessary = do
    socketExists <- fileExist ".hdevtools.sock"
    when (socketExists) $ do
        hdevSocketModTime <- modificationTime <$> getSymbolicLinkStatus ".hdevtools.sock"
        resultModTime <- modificationTime <$> getSymbolicLinkStatus "result"
        when (resultModTime >= hdevSocketModTime) $ do
            (_ec, out, err) <- readProcessWithExitCode "./result/bin/load-env-nhc-build" []
                [i|hdevtools --stop-server|]
            putStrLn out
            hPutStrLn stderr err
