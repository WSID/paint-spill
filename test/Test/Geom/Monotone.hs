{-# LANGUAGE OverloadedLists #-}

module Test.Geom.Monotone where

import Control.Applicative
import Data.List.NonEmpty

import Test.Tasty
import Test.Tasty.HUnit
import Test.Geom.Util

import Linear

import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Util

testGroupMonotone :: TestTree
testGroupMonotone = testGroup "Monotone"
  [ testGroup "xstripY"
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
      [ testCase "Triangle" $ do
            let monotone = XMonotone (V2 1 0) [Up (V2 0 1)] (V2 0 0)
            xmonoY monotone 0 @?= (0, 1)
            xmonoY monotone 1 @?= (0, 0)
      , testCase "Simple" $ do
            let monotone = XMonotone (V2 1 0) [Up (V2 0 1), Down (V2 0 (-1))] (V2 (-1) 0)
            xmonoY monotone 0 @?= ((-1), 1)
            xmonoY monotone 0.5 @?= ((-0.5), 0.5)
      , testCase "End Point" $ do
            let monotone = XMonotone (V2 1 0) [Up (V2 0 1), Down (V2 0 (-1))] (V2 (-1) 0)
            xmonoY monotone 1 @?= (0, 0)
            xmonoY monotone (-1) @?= (0, 0)
      , testCase "Vertical" $ do
            let monotone = XMonotone
                    (V2 1 2)
                    [Up (V2 0 2), Up (V2 0 3), Down (V2 0 0), Down (V2 0 1)]
                    (V2 (-1) 2)
            xmonoY monotone 0 @?= (0, 3)
      , testCase "More" $ do
            let monotone = XMonotone
                    (V2 3 2)
                    [Up (V2 2 4), Down (V2 1 0), Down (V2 0 0), Up (V2 (-1) 1), Down (V2 (-1) (-2))]
                    (V2 (-2) (-2))
            xmonoY monotone (-1.5) @?= ((-2), (-0.5))
            xmonoY monotone (-0.5) @?= ((-1), 1.5)
            xmonoY monotone 1 @?= (0, 3)
      ]

  , testGroup "monoElem"
      [ testCase "Simple" $ do
            let monotone = XMonotone (V2 1 0) [Up (V2 0 1), Down (V2 0 (-1))] (V2 (-1) 0)
            --  1 |    *
            --  0 |  * x *
            -- -1 |  x *
            xmonoElem monotone (V2 0 0) @?= True
            xmonoElem monotone (V2 (-1) (-1)) @?= False
            xmonoElem monotone (V2 3 0) @?= False
      , testCase "More" $ do
            let monotone = XMonotone
                    (V2 3 2)
                    [Up (V2 2 4), Down (V2 1 0), Down (V2 0 0), Up (V2 (-1) 1), Down (V2 (-1) (-2))]
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
            xmonoElem monotone (V2 4 2) @?= False
            xmonoElem monotone (V2 (-3) (-2)) @?= False 
      ]

  , testGroup "triangulateMonotone"
      [ testCase "Triangle" $ do
            let monotone = XMonotone (V2 1 0) [Up (V2 0 1)] (V2 0 0)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap

      , testCase "Triangle Low" $ do
            let monotone = XMonotone (V2 1 0) [Down (V2 0 (-1))] (V2 0 0)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap

      , testCase "Upside Zigs" $ do
            let monotone = XMonotone (V2 3 0) [Up (V2 2 2), Up (V2 1 1), Up (V2 (-1) 2), Up (V2 (-2) 2)] (V2 (-3) 0)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap
    
      , testCase "Downside Zigs" $ do
            let monotone = XMonotone (V2 3 3) [Down (V2 2 1), Down (V2 1 2), Down (V2 (-1) 1), Down (V2 (-2) 1)] (V2 (-3) 3)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap

      , testCase "Upside Zigs 1" $ do
            let monotone = XMonotone
                    (V2 3 0)
                    [Up (V2 2 2), Up (V2 1 1), Down (V2 0 (-1)), Up (V2 (-1) 2), Up (V2 (-2) 2)]
                    (V2 (-3) 0)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap
    
      , testCase "Downside Zigs 1" $ do
            let monotone = XMonotone
                    (V2 3 3)
                    [Down (V2 2 1), Down (V2 1 2), Up (V2 0 4), Down (V2 (-1) 1), Down (V2 (-2) 1)]
                    (V2 (-3) 3)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap

      , testCase "Simple" $ do
            let monotone = XMonotone
                    (V2 1 0)
                    [Up (V2 0 1), Down (V2 0 (-1))]
                    (V2 (-1) 0)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap

      , testCase "Vertical" $ do
            let monotone = XMonotone
                    (V2 1 2)
                    [Up (V2 0 2), Up (V2 0 3), Down (V2 0 0), Down (V2 0 1)]
                    (V2 (-1) 2)
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap

      , testCase "More" $ do
            let monotone = XMonotone
                    (V2 3 2)
                    [Up (V2 2 4), Down (V2 1 0), Down (V2 0 0), Up (V2 (-1) 1), Down (V2 (-1) (-2))]
                    (V2 (-2) (-2))
                triangles = triangulateXMono monotone

                monoMap = xmonoElem monotone <$> ccoordList
                triMaps = fmap (\t -> triElem t <$> ccoordList) triangles
                triMap = foldr1 (liftA2 (||)) triMaps

            monoMap @#=? triMap
      ]
  ]