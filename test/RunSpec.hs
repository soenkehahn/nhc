{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module RunSpec where


import Test.Hspec

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception
import System.Directory
import System.IO.Silently
import System.IO.Temp
import System.Process
import System.FilePath
import System.Exit
import System.IO
import System.Posix.IO

import Run


main :: IO ()
main = hspec spec

-- | executes the given function inside the safe example project
insideBifunctors :: IO () -> IO ()
insideBifunctors = insideExample "bifunctors"

insideExample :: String -> IO () -> IO ()
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

    it "executes cabal build" $ insideBifunctors $ do
      _ <- capture $ run' $ words "cabal build"
      return ()

    it "executes cabal test" $ insideBifunctors $ do
      _ <- capture $ run' $ words "cabal test"
      return ()
