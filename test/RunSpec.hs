{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module RunSpec where


import           Test.Hspec
import           Test.QuickCheck           hiding (Result)

import           Control.Applicative
import           Control.Concurrent.Thread
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.String.Interpolate
import           Safe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.IO.Temp
import           System.Posix.IO
import           System.Process

import           Run
import           Utils


main :: IO ()
main = hspec spec

-- | executes the given function inside the safe example project
insideBifunctors :: IO a -> IO a
insideBifunctors = insideExample "bifunctors"

insideExample :: String -> IO a -> IO a
insideExample name action = withSystemTempDirectory "nhc-test-suite" $
    \ tmpDir -> bracket (start tmpDir) end (const action)
  where
    start tmpDir = do
      outerDirectory <- getCurrentDirectory
      ExitSuccess <- system ("cp -r test/examples/" ++ name ++ " " ++ tmpDir)
      setCurrentDirectory (tmpDir </> name)
      return outerDirectory
    end outerDirectory = do
      setCurrentDirectory outerDirectory

createPipeHandles :: IO (Handle, Handle)
createPipeHandles = do
  (read, write) <- createPipe
  (,) <$> fdToHandle read <*> fdToHandle write


run' :: [String] -> IO ExitCode
run' command = run command (stdin, stdout)

spec :: Spec
spec = do

  describe "run" $ do
    it "executes normal shell commands" $ insideBifunctors $ do
      (output, _) <- capture $ run' ["echo", "foo"]
      lines output `shouldContain` ["foo"]

    it "executes runhaskell with needed dependencies in place" $ insideBifunctors $ do
      (output, _) <- capture $ run' $ words "runhaskell Main.hs"
      lines output `shouldContain` ["(2,0)"]

    it "returns the same exit code as the executed command" $ insideBifunctors $ do
      ec1 <- run' $ words "true"
      ec1 `shouldBe` ExitSuccess
      ec2 <- run' $ words "false"
      ec2 `shouldBe` ExitFailure 1

    it "executes interactive commands" $ insideBifunctors $ do
      (readEndStdin, writeEndStdin) <- createPipeHandles
      (readEndStdout, writeEndStdout) <- createPipeHandles
      _ <- forkIO $ void $ run (words "./interactive_command.sh") (readEndStdin, writeEndStdout)
      let waitForStarted = do
            l <- hGetLine readEndStdout
            when (l /= "started") waitForStarted
      waitForStarted
      hPutStrLn writeEndStdin ""
      l2 <- hGetLine readEndStdout
      l2 `shouldBe` "ending"

    it "executes ghci" $ insideBifunctors $ do
      (readEndStdin, writeEndStdin) <- createPipeHandles
      wait :: IO (Result String) <- snd <$> (forkIO $ capture_ $ run (words "ghci") (readEndStdin, stdout))
      mapM_ (hPutStrLn writeEndStdin) $
        "import Data.Bifunctor" :
        "bimap pred succ (1, 1)" :
        []
      hClose writeEndStdin
      output <- result =<< wait
      lines output `shouldContain` ["(0,2)"]

    it "sets an nhc-specific environment variable (for changing the prompt of invoked shells)" $ insideBifunctors $ do
      cabalFile <- capture_ $ run' (words "echo $NHC_CABAL_FILE")
      fmap takeFileName (lastMay (lines cabalFile))
        `shouldBe` Just "bifunctors-example.cabal"

    it "provides a decent error message when the command cannot be found" $ insideBifunctors $ do
      output <- hCapture_ [stderr] $ run' $ words "does_not_exist"
      output `shouldSatisfy` ("does_not_exist: command not found" `isInfixOf`)

    it "has a --help command line option" $ insideBifunctors $ do
      output <- capture_ $ run' $ words "--help"
      mapM_ (\ word -> (word `shouldSatisfy` (`isInfixOf` output))) ["execute", "COMMAND", "nix", "environment", "cabal", "http"]

    it "executes cabal build" $ insideBifunctors $ do
      _ <- capture $ run' $ words "cabal build"
      return ()

    it "executes cabal test" $ insideBifunctors $ do
      _ <- capture $ run' $ words "cabal test"
      return ()

    it "complains properly about unknown command line flags (to nhc)" $ insideBifunctors $ do
      (result, exitCode) <- hCapture [stderr] $ run' $ words "--does-not-exist"
      exitCode `shouldSatisfy` (/= ExitSuccess)
      lines result `shouldContain` ["Unknown flag --does-not-exist"]

    it "creates no non-hidden files or directories" $ insideBifunctors $ do
      before <- getDirectoryContents "."
      _ <- run' $ ["true"]
      after <- getDirectoryContents "."
      after \\ before `shouldBe` [".nhc"]

    it "rebuilds the environment to include libraries added to the cabal file" $ insideBifunctors $ do
      _ <- run' $ words "cabal build"
      _ <- system "cp added-dependency/* ."
      exitCode <- run' $ words "cabal build"
      exitCode `shouldBe` ExitSuccess
      return ()

    it "passes argument that contain spaces correctly to the invoked command" $
      insideBifunctors $ do
        output <- capture_ $ run' $ ["runhaskell", "PrintArgs.hs", "foo bar"]
        lines output `shouldContain` ["[\"foo bar\"]"]

    it "allows profiling" $ insideBifunctors $ do
      run' (words "--prof ghc -prof Main.hs") `shouldReturn` ExitSuccess
      run' (words "./Main +RTS -p") `shouldReturn` ExitSuccess

    it "does not allow profiling without --prof" $ insideBifunctors $ do
      exitCode <- run' (words "ghc -prof Main.hs")
      exitCode `shouldSatisfy` (/= ExitSuccess)

    it "does rebuild the environment when profiling flags toggles" $ insideBifunctors $ do
      exitCode <- run' (words "ghc -prof Main.hs")
      exitCode `shouldSatisfy` (/= ExitSuccess)
      run' (words "--prof ghc -prof Main.hs") `shouldReturn` ExitSuccess
      exitCode <- run' (words "ghc -prof Main.hs")
      exitCode `shouldSatisfy` (/= ExitSuccess)

    it "allows to use a custom 'default.nix' file" $ insideBifunctors $ do
      writeFile "custom.nix" $ normalizeLines [i|
          { pkgs ? import <nixpkgs> {},
            src ? ./.
          }:
          pkgs.haskellPackages.buildLocalCabal src "bifunctors-example"
        |]
      exitCode <- run' $ words "--custom-default=custom.nix runhaskell Main.hs"
      exitCode `shouldBe` ExitSuccess

    it "does not modify stdout of the executed commands \
       \(it should put all nhc-related output to stderr)" $ do
      property $ forAll (listOf (elements ['a' .. 'z'])) $ \ s -> ioProperty $ insideBifunctors $ do
        (output, ExitSuccess) <- capture $ run' $ ("echo" : s : [])
        return (output === s ++ "\n")
