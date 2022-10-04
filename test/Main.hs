module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Geom

main = defaultMain $ testGroup "Paint Spill"
  [ testGeom
  ]