{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec where


import Test.Hspec

import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Directory
import System.IO.Silently
import System.IO.Temp
import System.Process
import System.FilePath
import System.Exit

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


spec :: Spec
spec = do

  describe "run" $ do
    it "executes normal shell commands" $ insideBifunctors $ do
      (output, _) <- capture $ run ["echo", "foo"]
      lines output `shouldContain` ["foo"]

    it "executes runhaskell with needed dependencies in place" $ insideBifunctors $ do
      (output, _) <- capture $ run $ words "runhaskell Main.hs"
      lines output `shouldContain` ["(2,0)"]

    it "returns the same exit code as the executed command" $ insideBifunctors $ do
      ec1 <- run $ words "true"
      ec1 `shouldBe` ExitSuccess
      ec2 <- run $ words "false"
      ec2 `shouldBe` ExitFailure 1

    it "streams stdout and stderr of the child process" $ insideBifunctors $ do
      (output, _) <- capture $ do
        _ <- forkIO $ void $ run $ words "echo first" --  ; sleep 1"
        putStrLn "second"
        threadDelay 1200000
      filter (`elem` ["first", "second"]) (lines output)
        `shouldBe` ["first", "second"]

    it "executes cabal build" $ insideBifunctors $ do
      _ <- capture $ run $ words "cabal build"
      return ()

    it "executes cabal test" $ insideBifunctors $ do
      _ <- capture $ run $ words "cabal test"
      return ()
