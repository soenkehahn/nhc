
module RunSpec where


import Test.Hspec
import Control.Exception
import System.Directory
import System.IO.Silently

import Run


main :: IO ()
main = hspec spec

-- | executes the given function inside the safe example project
insideSafe :: IO () -> IO ()
insideSafe = withWorkingDirectory "./test/safe-example"

withWorkingDirectory :: FilePath -> IO () -> IO ()
withWorkingDirectory dir action = bracket start end (const action)
  where
    start = do
      tmp <- getCurrentDirectory
      setCurrentDirectory dir
      return tmp
    end tmp = do
      setCurrentDirectory tmp


spec :: Spec
spec = do
  describe "run" $ do
    it "allows you to run normal shell commands" $ insideSafe $ do
      (output, _) <- capture $ run ["echo", "foo"]
      output `shouldBe` "foo\n"

    it "allows to execute runhaskell with needed dependencies in place" $ insideSafe $ do
      (output, _) <- capture $ run $ words "runhaskell Main.hs"
      output `shouldBe` "Nothing\n"
