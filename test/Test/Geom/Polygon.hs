module Test.Geom.Polygon where

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit
import Test.Geom.Util ( ccoordList, (@#=?) )

import Linear

import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Geom.Monotone
import Graphics.PaintSpill.Geom.Polygon


testGroupPolygon :: TestTree
testGroupPolygon = testGroup "Polygon"
  [ testGroup "Poly Elem"
      [ testCase "Triangle" $ do
            let poly = [[V2 0 0, V2 1 0, V2 0 1]]
            
            V2 0    0    `polyElemNonZero` poly @?= True
            V2 0    1    `polyElemNonZero` poly @?= True
            V2 0    2    `polyElemNonZero` poly @?= False
            V2 1    0    `polyElemNonZero` poly @?= True
            V2 0.25 0.25 `polyElemNonZero` poly @?= True
            V2 1 1       `polyElemNonZero` poly @?= False
      , testCase "Diamond" $ do
            let poly = [[V2 0 1, V2 1 0, V2 2 1, V2 1 2]]

            V2 0 1 `polyElemNonZero` poly @?= True
            V2 2 1 `polyElemNonZero` poly @?= True
            V2 1 2 `polyElemNonZero` poly @?= True
            V2 1 0 `polyElemNonZero` poly @?= True
            V2 1 1 `polyElemNonZero` poly @?= True
            V2 0.5 0.5 `polyElemNonZero` poly @?= True
            V2 0 2 `polyElemNonZero` poly @?= False

      , testCase "C Shaped" $ do
            let poly = [[V2 0 0, V2 2 0, V2 1 1, V2 2 2, V2 0 2]]

            V2 0 0 `polyElemNonZero` poly @?= True
            V2 2 0 `polyElemNonZero` poly @?= True
            V2 1 1 `polyElemNonZero` poly @?= True
            V2 2 2 `polyElemNonZero` poly @?= True
            V2 0 2 `polyElemNonZero` poly @?= True
            V2 0.5 1 `polyElemNonZero` poly @?= True
            V2 2 1 `polyElemNonZero` poly @?= False
      
      , testCase "Holed Shape" $ do
            let poly =
                  [ [ V2 0 0, V2 3 0, V2 3 3, V2 0 3 ]
                  , [ V2 1 1, V2 1 2, V2 2 2, V2 2 1 ]
                  ]

            V2 0.5 0.5 `polyElemNonZero` poly @?= True
            V2 0.5 1.5 `polyElemNonZero` poly @?= True
            V2 1.5 1.5 `polyElemNonZero` poly @?= False
            V2 1.5 2.5 `polyElemNonZero` poly @?= True
            V2 2.5 2.5 `polyElemNonZero` poly @?= True
      ]

  , testGroup "Monotone Decomposition"
      [ testCase "Triangle" $ do
            let poly = [V2 0 0, V2 1 0, V2 0 1]
                monos = monotoneDecomp [zip [ 0 .. ] poly]
            
                polyMap = flip polyElemNonZero [poly] <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

            polyMap @#=? monoMap
            
      , testCase "Vase shaped" $ do
            let poly = [V2 0 0, V2 3 0, V2 2 1, V2 3 2, V2 0 2, V2 1 1]
                monos = monotoneDecomp [zip [ 0 .. ] poly]
            
                polyMap = flip polyElemNonZero [poly] <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps
            
            polyMap @#=? monoMap
            
      , testCase "Clover" $ do
            let poly = [V2 0 0, V2 1 0, V2 2 1, V2 3 0, V2 4 0, V2 4 1, V2 3 2, V2 4 3, V2 4 4, V2 3 4, V2 2 3, V2 1 4, V2 0 4, V2 0 3, V2 1 2, V2 0 1]
                monos = monotoneDecomp [zip [ 0 .. ] poly]
            
                polyMap = flip polyElemNonZero [poly] <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

            polyMap @#=? monoMap
            
      , testCase "ZigZags" $ do
            let poly = [V2 0 0, V2 2 0, V2 4 1, V2 2 2, V2 4 3, V2 2 4, V2 4 5, V2 2 6, V2 4 7, V2 2 7, V2 0 6, V2 2 5, V2 0 4, V2 2 3, V2 0 2, V2 2 1]
                monos = monotoneDecomp [zip [ 0 .. ] poly]
            
                polyMap = flip polyElemNonZero [poly] <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

            polyMap @#=? monoMap
            
      , testCase "ZigZags 2" $ do
            let poly = [V2 0 0, V2 3 0, V2 5 1, V2 3 2, V2 5 3, V2 3 4, V2 5 5, V2 3 6, V2 5 7, V2 2 7, V2 0 6, V2 2 5, V2 0 4, V2 2 3, V2 0 2, V2 2 1]
                monos = monotoneDecomp [zip [ 0 .. ] poly]
            
                polyMap = flip polyElemNonZero [poly] <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

            polyMap @#=? monoMap
            
      , testCase "Swirl Shape" $ do
            let poly = [V2 0 0, V2 2 0, V2 3 2, V2 4 0, V2 4 2, V2 2 3, V2 4 4, V2 2 4, V2 1 2, V2 0 4, V2 0 2, V2 2 1]
                monos = monotoneDecomp [zip [ 0 .. ] poly]
            
                polyMap = flip polyElemNonZero [poly] <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

            polyMap @#=? monoMap

      , testCase "Holed Shape" $ do
            let poly = 
                  [ [ V2 0 0, V2 3 0, V2 3 3, V2 0 3 ]
                  , [ V2 1 1, V2 1 2, V2 2 2, V2 2 1 ]
                  ]
                polyL =
                  [ [ (0, V2 0 0), (1, V2 3 0), (2, V2 3 3), (3, V2 0 3) ]
                  , [ (4, V2 1 1), (5, V2 1 2), (6, V2 2 2), (7, V2 2 1) ]
                  ]
                  
            let monos = monotoneDecomp polyL
            
                polyMap = flip polyElemNonZero poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

            polyMap @#=? monoMap

      ]
  ]