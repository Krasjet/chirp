{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text      as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy as LT
import qualified Test.Tasty as T

import Libkst.Hash
import Libkst.IO
import Libkst.Monad

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
  testIO1 <- testSpec "IO1" ioSpec1
  testIO2 <- testSpec "IO2" ioSpec2
  defaultMain $ testGroup "Tests"
    [ testHash
    , testIO1
    , T.after AllFinish "IO1" testIO2 -- can't be run in parallel
    ]
