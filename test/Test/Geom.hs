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
                let triangle = unindexedTriangle (V2 0 0) (V2 1 0) (V2 0 1)
                triElem triangle (V2 0.25 0.25) @?= True
                triElem triangle (V2 1.0 1.0) @?= False
          , testCase "Triangle" $ do
                let triangle = unindexedTriangle (V2 0 0) (V2 1 0) (V2 0.5 0.8)
                triElem triangle (V2 0.5 0.5) @?= True 
                triElem triangle (V2 0 1) @?= False
          , testCase "Any Triangle" $ do
                let triangle = unindexedTriangle (V2 0 1) (V2 1 0) (V2 2 2)
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
      , testGroup "segCacheY"
          [ testCase "A" (segCacheY (makeSegCache (V2 (-1) 0) (V2 1 0)) 0 @?= Just 0)
          , testCase "B" (segCacheY (makeSegCache (V2 0 0) (V2 2 2)) 1 @?= Just 1)
          , testCase "C" (segCacheY (makeSegCache (V2 0 0) (V2 0 2)) 0 @?= Nothing)
          ]
      , testGroup "segIntersect"
          [ testCase "Vertical Parallel" (segIntersect (V2 0 0) (V2 0 1) (V2 1 0) (V2 1 1) @?= Nothing)
          , testCase "Horizontal Parallel" (segIntersect (V2 0 0) (V2 1 0) (V2 0 1) (V2 1 1) @?= Nothing)
          , testCase "Diagonal Parallel" (segIntersect (V2 0 0) (V2 1 1) (V2 1 0) (V2 2 1) @?= Nothing)
          , testCase "X cross" (segIntersect (V2 0 0) (V2 1 1) (V2 0 1) (V2 1 0) @?= Just (V2 0.5 0.5))
          , testCase "A" (segIntersect (V2 0 0) (V2 1 4) (V2 0 3) (V2 1 3) @?= Just (V2 0.75 3))
          , testCase "B" (segIntersect (V2 0 0) (V2 2 3) (V2 3 2) (V2 0 0) @?= Just (V2 0 0))
          , testCase "Vertical 1" (segIntersect (V2 0 1) (V2 2 3) (V2 1 0) (V2 1 4) @?= Just (V2 1 2))
          , testCase "Vertical 2" (segIntersect (V2 2 0) (V2 2 4) (V2 1 0) (V2 3 4) @?= Just (V2 2 2))
          ]
      , testGroup "segCacheIntersect"
          [ testCase "Vertical Parallel" (segCacheIntersect (SegVertical 0) (SegVertical 1) @?= Nothing)
          , testCase "Horizontal Parallel" (segCacheIntersect (SegCache 0 0) (SegCache 0 1) @?= Nothing)
          , testCase "Diagonal Parallel" (segCacheIntersect (SegCache 1 0) (SegCache 1 1) @?= Nothing)
          , testCase "X cross" (segCacheIntersect (SegCache 1 0) (SegCache (-1) 1) @?= Just (V2 0.5 0.5))
          , testCase "A" (segCacheIntersect (SegCache 4   0) (SegCache 0 3) @?= Just (V2 0.75 3))
          , testCase "B" (segCacheIntersect (SegCache 1.5 0) (SegCache (2 / 3) 0) @?= Just (V2 0 0))
          , testCase "Vertical 1" (segCacheIntersect (SegCache 1 1) (SegVertical 1) @?= Just (V2 1 2))
          , testCase "Vertical 2" (segCacheIntersect (SegVertical 2) (SegCache 2 (-2)) @?= Just (V2 2 2))
          ]
      ]
  , testGroupMonotone
  , testGroupPolygon
  ]