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
  , testGroup "Monotone Decomposition"
      [ testCase "Triangle" $ do
            let poly = [V2 0 0, V2 1 0, V2 0 1]
                monos = fmap (fmap snd) $ monotoneDecomp (zip [ 0 .. ] poly)
            
                polyMap = flip polyElem poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

                npolyverts = length poly
                nmonoverts = sum (fmap (\(XMonotone _ s _) -> length s + 2) monos) - (length monos * 2) + 2
            
            polyMap @#=? monoMap
            npolyverts @=? nmonoverts
            
      , testCase "Vase shaped" $ do
            let poly = [V2 0 0, V2 3 0, V2 2 1, V2 3 2, V2 0 2, V2 1 1]
                monos = fmap (fmap snd) $ monotoneDecomp (zip [ 0 .. ] poly)
            
                polyMap = flip polyElem poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

                npolyverts = length poly
                nmonoverts = sum (fmap (\(XMonotone _ s _) -> length s + 2) monos) - (length monos * 2) + 2
            
            polyMap @#=? monoMap
            npolyverts @=? nmonoverts
            
      , testCase "Clover" $ do
            let poly = [V2 0 0, V2 1 0, V2 2 1, V2 3 0, V2 4 0, V2 4 1, V2 3 2, V2 4 3, V2 4 4, V2 3 4, V2 2 3, V2 1 4, V2 0 4, V2 0 3, V2 1 2, V2 0 1]
                monos = fmap (fmap snd) $ monotoneDecomp (zip [ 0 .. ] poly)
            
                polyMap = flip polyElem poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

                npolyverts = length poly
                nmonoverts = sum (fmap (\(XMonotone _ s _) -> length s + 2) monos) - (length monos * 2) + 2

            print monos

            polyMap @#=? monoMap
            npolyverts @=? nmonoverts
            
      , testCase "ZigZags" $ do
            let poly = [V2 0 0, V2 2 0, V2 4 1, V2 2 2, V2 4 3, V2 2 4, V2 4 5, V2 2 6, V2 4 7, V2 2 7, V2 0 6, V2 2 5, V2 0 4, V2 2 3, V2 0 2, V2 2 1]
                monos = fmap (fmap snd) $ monotoneDecomp (zip [ 0 .. ] poly)
            
                polyMap = flip polyElem poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

                npolyverts = length poly
                nmonoverts = sum (fmap (\(XMonotone _ s _) -> length s + 2) monos) - (length monos * 2) + 2

            print monos

            polyMap @#=? monoMap
            npolyverts @=? nmonoverts
            
      , testCase "ZigZags 2" $ do
            let poly = [V2 0 0, V2 3 0, V2 5 1, V2 3 2, V2 5 3, V2 3 4, V2 5 5, V2 3 6, V2 5 7, V2 2 7, V2 0 6, V2 2 5, V2 0 4, V2 2 3, V2 0 2, V2 2 1]
                monos = fmap (fmap snd) $ monotoneDecomp (zip [ 0 .. ] poly)
            
                polyMap = flip polyElem poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

                npolyverts = length poly
                nmonoverts = sum (fmap (\(XMonotone _ s _) -> length s + 2) monos) - (length monos * 2) + 2

            print monos

            polyMap @#=? monoMap
            npolyverts @=? nmonoverts
            
      , testCase "Swirl Shape" $ do
            let poly = [V2 0 0, V2 2 0, V2 3 2, V2 4 0, V2 4 2, V2 2 3, V2 4 4, V2 2 4, V2 1 2, V2 0 4, V2 0 2, V2 2 1]
                monos = fmap (fmap snd) $ monotoneDecomp (zip [ 0 .. ] poly)
            
                polyMap = flip polyElem poly <$> ccoordList
                monoMaps = fmap (\m -> fmap (xmonoElem m) ccoordList) monos
                monoMap = foldr (liftA2 (||)) (False <$ ccoordList) monoMaps

                npolyverts = length poly
                nmonoverts = sum (fmap (\(XMonotone _ s _) -> length s + 2) monos) - (length monos * 2) + 2

            print monos

            polyMap @#=? monoMap
            npolyverts @=? nmonoverts
      ]
  ]