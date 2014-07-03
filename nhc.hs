#!/usr/bin/env runhaskell

{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}


import Control.Applicative
import Control.Monad
import Control.Exception
import Data.String.Interpolate
import System.Process (system, readProcessWithExitCode)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Environment


main :: IO ()
main =
    run =<< getArgs

run :: [String] -> IO ()
run hdevtoolsArgs = do
    -- prerequisites
    cabalFile <- getCabalFile
    defaultFile <- createDefaultNixFileIfMissing (takeBaseName cabalFile)
    nhcFile <- createNhcNixFileIfMissing
    -- building the environment
    nixBuild cabalFile nhcFile
    -- entering the environment and invoking hdevtools
    check hdevtoolsArgs


getCabalFile :: IO FilePath
getCabalFile = do
    fs <- filter ((== ".cabal") . takeExtension) <$>
        getDirectoryContents "."
    case fs of
        [f] -> return f

createDefaultNixFileIfMissing :: String -> IO FilePath
createDefaultNixFileIfMissing packageName = do
    exists <- fileExist "default.nix"
    when (not exists) $
        writeFile "default.nix" [i|
            { pkgs ? import <nixpkgs> {},
              src ? ./. } :
            {
                build = pkgs.haskellPackages.buildLocalCabal src "#{packageName}";
            }
          |]
    return "default.nix"

-- | Creates a file 'nhc.nix' that is used to build the environment for
-- checking the haskell sources. If the file already exists it is left
-- untouched. This allows for modifying the build environment, e.g. for
-- profiling.
createNhcNixFileIfMissing :: IO FilePath
createNhcNixFileIfMissing = do
    exists <- doesFileExist "nhc.nix"
    when (not exists) $ do
        writeFile "nhc.nix" $ [i|
            with builtins;

            let

                originalPkgs = import <nixpkgs> { config.allowUnfree = true; };
                pkgs = originalPkgs // {
                    haskellPackages = originalPkgs.haskellPackages_ghc763;
                };

                hsEnv = pkgs.haskellPackages.ghcWithPackages
                    (hsPkgs :
                     let package = (hsPkgs.callPackage ./default.nix { inherit pkgs; }).build;
                     in
                        [hsPkgs.hdevtools] ++
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
nixBuild :: FilePath -> FilePath -> IO ()
nixBuild cabalFile nhcFile = do
    resultExists <- fileExist "result"
    if not resultExists then run else do
        cabalModTime <- modificationTime <$> getSymbolicLinkStatus cabalFile
        nhcModTime <- modificationTime <$> getSymbolicLinkStatus nhcFile
        resultModTime <- modificationTime <$> getSymbolicLinkStatus "result"
        when (cabalModTime >= resultModTime ||
              nhcModTime >= resultModTime)
            run
  where
    run :: IO ()
    run = do
        hPutStrLn stderr "building..."
                                         -- yeah, I know, this is bad.
        ExitSuccess <- system "nix-build -I /home/shahn/zalora/nix nhc.nix -j4"
        return ()

-- | Performs the actual check
check :: [String] -> IO ()
check hdevtoolsArgs = do
    stopHdevtoolsIfNecessary
    (ec, out, err) <- readProcessWithExitCode "./result/bin/load-env-nhc-build" []
        [i|hdevtools #{unwords hdevtoolsArgs}|]
    putStrLn out
    hPutStrLn stderr err

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
            (ec, out, err) <- readProcessWithExitCode "./result/bin/load-env-nhc-build" []
                [i|hdevtools --stop-server|]
            putStrLn out
            hPutStrLn stderr err
