#!/usr/bin/env runhaskell

{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}


import Control.Applicative
import Control.Monad
import Control.Exception
import Options
import Data.String.Interpolate
import System.Process (system, readProcessWithExitCode)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files


data NhcOptions = NhcOptions {
    optMessage :: String,
    optQuiet :: Bool
  }
    deriving Show

instance Options NhcOptions where
    defineOptions = NhcOptions <$>
        simpleOption "message" "Hello world!"
            "A message to show the user." <*>
        simpleOption "quiet" False
            "Whether to be quiet."

main :: IO ()
main = runCommand $ \ opts args -> case args of
    (defaultFile : haskellFiles) -> run opts defaultFile haskellFiles
    _ -> error "missing or too many arguments"

run :: NhcOptions -> FilePath -> [FilePath] -> IO ()
run options defaultFile haskellFiles = do
    -- prerequisites
    cabalFile <- getCabalFile
    checkFileExists defaultFile
    nhcFile <- createNhcNixFileIfMissing
    --
    nixBuild cabalFile nhcFile
    --
    check haskellFiles


getCabalFile :: IO FilePath
getCabalFile = do
    fs <- filter ((== ".cabal") . takeExtension) <$>
        getDirectoryContents "."
    case fs of
        [f] -> return f

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
        ExitSuccess <- system "nix-build -I /home/shahn/zalora/nix nhc.nix"
        return ()

-- | Performs the actual check
check :: [FilePath] -> IO ()
check haskellFiles = do
    stopHdevtoolsIfNecessary
    forM_ haskellFiles $ \ haskellFile -> do
        hPutStrLn stderr [i|checking #{haskellFile}...|]
        (ec, out, err) <- readProcessWithExitCode "./result/bin/load-env-nhc-build" []
            [i|hdevtools check #{haskellFile}|]
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


-- utils

checkFileExists file = do
    exists <- doesFileExist file
    when (not exists) $
        throwIO $ ErrorCall ("file not found: " ++ file)
