module Main where

import Test.Tasty (defaultMain)
import Test.Tasty.HUnit

import Graphics.PaintSpill (answerOfEverything)

main = defaultMain tests
  where
    tests = testCase "Test Answer of Everything" $ do
        a <- answerOfEverything 
        a @=? 42