{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}

module Run where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.String.Interpolate
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process (system, readProcessWithExitCode)
import System.IO.Silently
import System.SetEnv


run :: [String] -> IO ExitCode
run command = do
    -- prerequisites
    cabalFile <- getCabalFile
    defaultFile <- createDefaultNixFileIfMissing (takeBaseName cabalFile)
    nhcFile <- createNhcNixFileIfMissing defaultFile
    -- building the environment
    nixBuild cabalFile nhcFile
    -- entering the environment and performing the given command
    performCommand command


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

                originalPkgs = import <nixpkgs> { config.allowUnfree = true; };
                pkgs = originalPkgs // {
                    haskellPackages = originalPkgs.haskellPackages_ghc763;
                };

                git_hdevtools_src = pkgs.fetchgit {
                    url = "https://github.com/maximkulkin/hdevtools.git";
                    rev = "b0b0c15ed2cad92dd3b88e609f69d492e75e2e98";
                    sha256 = "0c221c889c324aea15fe53528896447de8210bd519077258a99c681ca517ff5b";
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
        ExitSuccess <- system "nix-build nhc.nix -j4"
        return ()

-- | Performs the command inside the environment.
performCommand :: [String] -> IO ExitCode
performCommand command = do
    stopHdevtoolsIfNecessary
    unsetEnv "_PATH"
    ("", (ec, out, err)) <- capture $ readProcessWithExitCode "./result/bin/load-env-nhc-build" []
      (unwords command)
    putStrLn out
    hPutStrLn stderr err
    return ec

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


-- utils

-- | Normalizes lines as the nix lines literals do.
--
-- >>> normalizeLines " \n \n  foo\n\n  bar\n    baz\n \n"
-- "foo\n\nbar\n  baz\n"
normalizeLines :: String -> String
normalizeLines =
    lines >>>
    -- convert whitespace lines to empty lines
    map (\ line -> if all (== ' ') line then "" else line) >>>
    -- strip empty lines at start and end
    dropWhile null >>> reverse >>> dropWhile null >>> reverse >>>
    -- strip indentation
    stripIndentation >>>
    unlines
  where
    stripIndentation :: [String] -> [String]
    stripIndentation ls =
        let indent = minimum $
                map (length . takeWhile (== ' ')) $
                filter (not . null) ls
        in map (drop indent) ls
