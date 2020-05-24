{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text      as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy as LT
import qualified Test.Tasty as T

import Libkst.Hash
import Libkst.IO
import Libkst.Monad
import Libkst.Text.Parse

import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket_)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Hspec


-- * Utility functions

shouldContainFile
  :: HasCallStack
  => FilePath -- ^ Path
  -> FilePath -- ^ Directory
  -> Expectation
path `shouldContainFile` dir = do
  d <- liftIO $ listDirectory path
  d `shouldContain` [dir]

shouldNotContainFile
  :: HasCallStack
  => FilePath -- ^ Path
  -> FilePath -- ^ Directory
  -> Expectation
path `shouldNotContainFile` dir = do
  d <- liftIO $ listDirectory path
  d `shouldNotContain` [dir]

-- * Hash tests

hashSpec :: Spec
hashSpec = parallel $ do
  let str = "Hello, world!"
      expected = show (length str) <> "_" <> "MV9b23bQeMQ7isAGTkoBZGErH853yGk0W-yUx1iU7dM-"
  it "hashes lazy text correctly" $
    hashText (LT.pack str) `shouldBe` expected

  it "hashes text correctly" $
    hashText' (T.pack str) `shouldBe` expected

-- * Parse tests

parseSpec :: Spec
parseSpec = parallel $ do
  describe "skipTo" $ do
    it "skips to prefix" $ do
       skipTo "<xml" "<p><xml></xml>" `shouldBe` "<xml></xml>"
       skipTo "<xml" "<xml></xml>" `shouldBe` "<xml></xml>"

    it "returns empty string when no pattern is found" $ do
       skipTo "<xml" "<p></xml>" `shouldBe` ""
       skipTo "<xml" "" `shouldBe` ""

  describe "skipAfter" $ do
    it "skips after prefix" $ do
       skipAfter "<xml>" "<p><xml></xml>" `shouldBe` Just "</xml>"
       skipAfter "<xml>" "<xml></xml>" `shouldBe` Just "</xml>"
       skipAfter "\\begin{" "aaaa\\begin{env}bbbb" `shouldBe` Just "env}bbbb"

    it "returns Nothing when no pattern is found" $ do
       skipAfter "<xml>" "</xml>" `shouldBe` Nothing
       skipAfter "\\begin{" "aaaa\\end{env}bbbb" `shouldBe` Nothing
       skipAfter "\\begin{" "" `shouldBe` Nothing

  describe "takeUntil" $ do
    it "takes until suffix" $ do
       takeUntil "</xml>" "<p><xml></xml></p>" `shouldBe` "<p><xml>"
       takeUntil "</xml>" "<p><xml></xml>" `shouldBe` "<p><xml>"
       takeUntil "</xml>" "</xml></p>" `shouldBe` ""

    it "returns entire string if suffix not found" $ do
       takeUntil "</xml>" "<p><xml>" `shouldBe` "<p><xml>"
       takeUntil "</xml>" "" `shouldBe` ""

  describe "between'" $ do
    it "finds string between begin and end" $ do
       between' "<xml>" "</xml>" "aaaa<xml>text</xml>bbbb" `shouldBe` Just "text"
       between' "<xml>" "</xml>" "aaaa<xml>text</xml>" `shouldBe` Just "text"
       between' "<xml>" "</xml>" "<xml>text</xml>bbbb" `shouldBe` Just "text"
       between' "<xml>" "</xml>" "<xml>text</xml>" `shouldBe` Just "text"
       between' "\\begin{" "}" "\\begin{env}\\end{env}" `shouldBe` Just "env"
       between' "\\begin{" "}" "aaaa\\begin{env}\\end{env}bbbb" `shouldBe` Just "env"
       between' "\\begin{" "}" "aaaa\\begin{env}" `shouldBe` Just "env"

    it "returns Nothing prefix not found" $ do
       between' "\\begin{" "}" "\\end{env}" `shouldBe` Nothing
       between' "\\begin{" "}" "\\end{env}" `shouldBe` Nothing
       between' "\\begin{" "}" "aaaa\\end{env}" `shouldBe` Nothing
       between' "\\begin{" "}" "env}" `shouldBe` Nothing

    it "returns string between if suffix not found" $ do
       between' "\\begin{" "}" "aaaa\\begin{env" `shouldBe` Just "env"
       between' "\\begin{" "}" "\\begin{env{" `shouldBe` Just "env{"
       between' "<xml>" "</xml>" "<xml>text<xml>" `shouldBe` Just "text<xml>"

  describe "betweenC'" $ do
    it "finds string between begin and end" $ do
       betweenC' "\\begin{" '}' "\\begin{env}\\end{env}" `shouldBe` Just "env"
       betweenC' "\\begin{" '}' "aaaa\\begin{env}\\end{env}bbbb" `shouldBe` Just "env"
       betweenC' "\\begin{" '}' "aaaa\\begin{env}" `shouldBe` Just "env"

    it "returns Nothing prefix not found" $ do
       betweenC' "\\begin{" '}' "\\end{env}" `shouldBe` Nothing
       betweenC' "\\begin{" '}' "\\end{env}" `shouldBe` Nothing
       betweenC' "\\begin{" '}' "aaaa\\end{env}" `shouldBe` Nothing
       betweenC' "\\begin{" '}' "env}" `shouldBe` Nothing

    it "returns string between if suffix not found" $ do
       betweenC' "\\begin{" '}' "aaaa\\begin{env" `shouldBe` Just "env"
       betweenC' "\\begin{" '}' "\\begin{env{" `shouldBe` Just "env{"

  describe "between" $ do
    it "finds string between begin and end" $ do
       between "<xml>" "</xml>" "aaaa<xml>text</xml>bbbb" `shouldBe` Just "text"
       between "<xml>" "</xml>" "aaaa<xml>text</xml>" `shouldBe` Just "text"
       between "<xml>" "</xml>" "<xml>text</xml>bbbb" `shouldBe` Just "text"
       between "<xml>" "</xml>" "<xml>text</xml>" `shouldBe` Just "text"
       between "\\begin{" "}" "\\begin{env}\\end{env}" `shouldBe` Just "env"
       between "\\begin{" "}" "aaaa\\begin{env}\\end{env}bbbb" `shouldBe` Just "env"
       between "\\begin{" "}" "aaaa\\begin{env}" `shouldBe` Just "env"

    it "returns Nothing prefix not found" $ do
       between "\\begin{" "}" "\\end{env}" `shouldBe` Nothing
       between "\\begin{" "}" "\\end{env}" `shouldBe` Nothing
       between "\\begin{" "}" "aaaa\\end{env}" `shouldBe` Nothing
       between "\\begin{" "}" "env}" `shouldBe` Nothing

    it "returns Nothing between if suffix not found" $ do
       between "\\begin{" "}" "aaaa\\begin{env" `shouldBe` Nothing
       between "\\begin{" "}" "\\begin{env{" `shouldBe` Nothing
       between "<xml>" "</xml>" "<xml>text<xml>" `shouldBe` Nothing

  describe "betweenC" $ do
    it "finds string between begin and end" $ do
       betweenC "\\begin{" '}' "\\begin{env}\\end{env}" `shouldBe` Just "env"
       betweenC "\\begin{" '}' "aaaa\\begin{env}\\end{env}bbbb" `shouldBe` Just "env"
       betweenC "\\begin{" '}' "aaaa\\begin{env}" `shouldBe` Just "env"

    it "returns Nothing prefix not found" $ do
       betweenC "\\begin{" '}' "\\end{env}" `shouldBe` Nothing
       betweenC "\\begin{" '}' "\\end{env}" `shouldBe` Nothing
       betweenC "\\begin{" '}' "aaaa\\end{env}" `shouldBe` Nothing
       betweenC "\\begin{" '}' "env}" `shouldBe` Nothing

    it "returns Nothing if suffix not found" $ do
       betweenC "\\begin{" '}' "aaaa\\begin{env" `shouldBe` Nothing
       betweenC "\\begin{" '}' "\\begin{env{" `shouldBe` Nothing

-- * IO tests

testDir :: FilePath
testDir = "testDir"

tmpDir :: FilePath
tmpDir = "tmpDir1"

tmpFile :: FilePath
tmpFile = "tmp" <.> "txt"

withTestDir :: IO () -> IO ()
withTestDir = bracket_ createTestDir removeTestDir
  where
    createTestDir = do
      createDirectoryIfMissing True testDir
      whenM (doesDirectoryExist (testDir <> tmpDir)) $
        removeDirectoryRecursive (testDir <> tmpDir)

    removeTestDir =
      whenM (doesDirectoryExist testDir) $
        removeDirectoryRecursive testDir


ioSpec1 :: Spec
ioSpec1 = around_ withTestDir $
  describe "createParentDir" $
    it "creates missing directory and not file" $ do
      -- no dir before test
      testDir `shouldNotContainFile` tmpDir

      liftIO $ createParentDir (testDir </> tmpDir </> tmpFile)

      testDir `shouldContainFile` tmpDir
      (testDir </> tmpDir) `shouldNotContainFile` tmpFile

      removeDirectoryRecursive (testDir </> tmpDir)

      -- no dir after test
      testDir `shouldNotContainFile` tmpDir

ioSpec2 :: Spec
ioSpec2 = around_ withTestDir $
  describe "writeFileHandleMissing" $
    it "creates missing directory and file" $ do
      -- no dir before test
      testDir `shouldNotContainFile` tmpDir

      liftIO $ writeFileHandleMissing (testDir </> tmpDir </> tmpFile) "test"

      testDir `shouldContainFile` tmpDir
      (testDir </> tmpDir) `shouldContainFile` tmpFile

      t <- liftIO $ TIO.readFile (testDir </> tmpDir </> tmpFile)
      t `shouldBe` "test"

      removeDirectoryRecursive (testDir </> tmpDir)

      -- no dir after test
      testDir `shouldNotContainFile` tmpDir



main :: IO ()
main = do
  testHash <- testSpec "Hashing" hashSpec
  testParse <- testSpec "Parsing" parseSpec
  testIO1 <- testSpec "IO1" ioSpec1
  testIO2 <- testSpec "IO2" ioSpec2
  defaultMain $ testGroup "Tests"
    [ testHash
    , testParse
    , testIO1
    , T.after AllFinish "IO1" testIO2 -- can't be run in parallel, TODO switch to MVar instead
    ]
