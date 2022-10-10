module Test.Geom.Polygon where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Geom.Util

import Linear

import Graphics.PaintSpill.Geom


testGroupPolygon :: TestTree
testGroupPolygon = testGroup "Polygon"
  [ testGroup "Poly Elem"
      [ testCase "Triangle" $ do
            let poly = [V2 0 0, V2 1 0, V2 0 1]
            
            V2 0    0    `polyElem` poly @?= True
            V2 0    1    `polyElem` poly @?= True
            V2 0    2    `polyElem` poly @?= False
            V2 1    0    `polyElem` poly @?= True
            V2 0.25 0.25 `polyElem` poly @?= True
            V2 1 1       `polyElem` poly @?= False
      , testCase "Diamond" $ do
            let poly = [V2 0 1, V2 1 0, V2 2 1, V2 1 2]

            V2 0 1 `polyElem` poly @?= True
            V2 2 1 `polyElem` poly @?= True
            V2 1 2 `polyElem` poly @?= True
            V2 1 0 `polyElem` poly @?= True
            V2 1 1 `polyElem` poly @?= True
            V2 0.5 0.5 `polyElem` poly @?= True
            V2 0 2 `polyElem` poly @?= False

      , testCase "C Shaped" $ do
            let poly = [V2 0 0, V2 2 0, V2 1 1, V2 2 2, V2 0 2]

            V2 0 0 `polyElem` poly @?= True
            V2 2 0 `polyElem` poly @?= True
            V2 1 1 `polyElem` poly @?= True
            V2 2 2 `polyElem` poly @?= True
            V2 0 2 `polyElem` poly @?= True
            V2 0.5 1 `polyElem` poly @?= True
            V2 2 1 `polyElem` poly @?= False
      ]
  , testGroup "Monotone Decomposition" []
  ]