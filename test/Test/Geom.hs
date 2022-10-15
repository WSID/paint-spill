module Test.Geom where

import Data.Bool

import Test.Tasty
import Test.Tasty.HUnit
import Test.Geom.Util
import Test.Geom.Monotone
import Test.Geom.Polygon

import Linear

import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Geom.Triangle


testGeom :: TestTree
testGeom = testGroup "Geom"
  [ testGroup "Triangles"
      [ testGroup "triElem"
          [ testCase "Simple" $ do
                let triangle = Triangle (V2 0 0) (V2 1 0) (V2 0 1)
                triElem triangle (V2 0.25 0.25) @?= True
                triElem triangle (V2 1.0 1.0) @?= False
          , testCase "Triangle" $ do
                let triangle = Triangle (V2 0 0) (V2 1 0) (V2 0.5 0.8)
                triElem triangle (V2 0.5 0.5) @?= True 
                triElem triangle (V2 0 1) @?= False
          , testCase "Any Triangle" $ do
                let triangle = Triangle (V2 0 1) (V2 1 0) (V2 2 2)
                triElem triangle (V2 1 1) @?= True 
                triElem triangle (V2 0 0) @?= False
          ]
      , testGroup "segY"
          [ testGroup "Up"
              [ testCase "A" (segYUp (V2 (-1) 0) (V2 1 0) 0 @?= 0)
              , testCase "B" (segYUp (V2 0 0) (V2 2 2) 1 @?= 1)
              , testCase "C" (segYUp (V2 1 1) (V2 1 1) 1 @?= 1)
              , testCase "D" (segYUp (V2 0 0) (V2 0 2) 0 @?= 2)
              ]
          , testGroup "Down"
              [ testCase "A" (segYDown (V2 (-1) 0) (V2 1 0) 0 @?= 0)
              , testCase "B" (segYDown (V2 0 0) (V2 2 2) 1 @?= 1)
              , testCase "C" (segYDown (V2 1 1) (V2 1 1) 1 @?= 1)
              , testCase "D" (segYDown (V2 0 0) (V2 0 2) 0 @?= 0)
              ]
          ]
      ]
  , testGroupMonotone
  , testGroupPolygon
  ]