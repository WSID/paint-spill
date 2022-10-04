module Test.Geom where

import Test.Tasty
import Test.Tasty.HUnit

import Linear

import Graphics.PaintSpill.Geom

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

      , testGroup "xstripY"
          [ testGroup "Up"
              [ testCase "OnSegment" $ do
                    let start = V2 (-3) 0
                        end = V2 3 1
                        strip = [V2 1 0, V2 (-1) (-2)]
                    xstripYUp end strip start 0 @?= (-1)
              , testCase "OnPoint" $ do
                    let start = V2 (-2) 0
                        end = V2 4 1
                        strip = [V2 2 0, V2 0 (-2)]
                    xstripYUp end strip start 0 @?= -2
              , testCase "OnVertical" $ do
                    let start = V2 (-2) 0
                        end = V2 2 1
                        strip = [V2 0 0, V2 0 2]
                    xstripYUp end strip start 0 @?= 2
              ]

          , testGroup "Down"
              [ testCase "OnSegment" $ do
                    let start = V2 (-3) 0
                        end = V2 3 1
                        strip = [V2 1 0, V2 (-1) (-2)]
                    xstripYDown end strip start 0 @?= (-1)
              , testCase "OnPoint" $ do
                    let start = V2 (-2) 0
                        end = V2 4 1
                        strip = [V2 2 0, V2 0 (-2)]
                    xstripYDown end strip start 0 @?= -2
              , testCase "OnVertical" $ do
                    let start = V2 (-2) 0
                        end = V2 2 1
                        strip = [V2 0 0, V2 0 2]
                    xstripYDown end strip start 0 @?= 0
              ]
          ]

      , testGroup "xmonoY"
          [ testCase "Simple" $ do
                let monotone = XMonotone (V2 1 0) [V2 0 1] [V2 0 (-1)] (V2 (-1) 0)
                xmonoY monotone 0 @?= ((-1), 1)
                xmonoY monotone 0.5 @?= ((-0.5), 0.5)
          , testCase "End Point" $ do
                let monotone = XMonotone (V2 1 0) [V2 0 1] [V2 0 (-1)] (V2 (-1) 0)
                xmonoY monotone 1 @?= (0, 0)
                xmonoY monotone (-1) @?= (0, 0)
          , testCase "More" $ do
                let monotone = XMonotone
                        (V2 3 2)
                        [V2 2 4, V2 (-1) 1]
                        [V2 1 0, V2 0 0, V2 (-1) (-2)]
                        (V2 (-2) (-2))
                xmonoY monotone (-1.5) @?= ((-2), (-0.5))
                xmonoY monotone (-0.5) @?= ((-1), 1.5)
                xmonoY monotone 1 @?= (0, 3)
          ]

      , testGroup "monoElem"
          [ testCase "Simple" $ do
                let monotone = XMonotone (V2 1 0) [V2 0 1] [V2 0 (-1)] (V2 (-1) 0)
                --  1 |    *
                --  0 |  * x *
                -- -1 |  x *
                xmonoElem monotone (V2 0 0) @?= True
                xmonoElem monotone (V2 (-1) (-1)) @?= False
          , testCase "More" $ do
                let monotone = XMonotone
                        (V2 3 2)
                        [V2 2 4, V2 (-1) 1]
                        [V2 1 0, V2 0 0, V2 (-1) (-2)]
                        (V2 (-2) (-2))
                --  4 |     |   *
                --  3 |     |
                --  2 |     |     *
                --  1 |   * |
                --  0 | ----*-*----
                -- -1 |     |
                -- -2 | * * |
                --     -2-1 0 1 2 3
                xmonoElem monotone (V2 (-1) (-1)) @?= True
                xmonoElem monotone (V2 2 0) @?= False
                xmonoElem monotone (V2 2 2) @?= True
                xmonoElem monotone (V2 (-1) 2) @?= False
          ]
      ]
  ]