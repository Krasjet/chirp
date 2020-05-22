{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

import Libkst.Hash

import Test.Tasty
import Test.Tasty.Hspec

hashSpec :: Spec
hashSpec = parallel $ do
  let str = "Hello, world!"
      expected = show (length str) <> "_" <> "MV9b23bQeMQ7isAGTkoBZGErH853yGk0W-yUx1iU7dM-"
  it "hashes lazy text correctly" $
    hashText (LT.pack str) `shouldBe` expected

  it "hashes text correctly" $
    hashText' (T.pack str) `shouldBe` expected

main :: IO ()
main = do
  testHash <- testSpec "Hashing" hashSpec
  defaultMain $ testGroup "Tests"
    [ testHash
    ]
