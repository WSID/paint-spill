module Bench.Geom where

import Data.Foldable
import Data.Maybe
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N

import Control.DeepSeq
import Gauge

import Linear
import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Geom.Monotone
import Graphics.PaintSpill.Geom.Polygon
import Graphics.PaintSpill.Geom.Triangle
import Graphics.PaintSpill.Util

-- A fibonacci list to make numeric patterns.
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

monotoneSmall :: XMonotone () (V2 Float)
monotoneSmall = force (unindexedXMonotone (V2 1 0) (Up (V2 0 1) :| []) (V2 0 0))

monotoneTopOnlyN :: Int -> XMonotone () (V2 Float)
monotoneTopOnlyN n = unindexedXMonotone (V2 (fromIntegral n + 1) 0) (fromJust $ nonEmpty u) (V2 0 0)
  where
    u = fmap Up $ zipWith V2
        (fmap (\i -> fromIntegral (n - i + 1)) [1 .. n])
        (fmap (\f -> fromIntegral ((f `mod` 5) + 1)) fibs)

monotoneTopOnly128 :: XMonotone () (V2 Float)
monotoneTopOnly128 = force (monotoneTopOnlyN 128)

monotoneTopOnly256 :: XMonotone () (V2 Float)
monotoneTopOnly256 = force (monotoneTopOnlyN 256)

monotoneTopOnly512 :: XMonotone () (V2 Float)
monotoneTopOnly512 = force (monotoneTopOnlyN 512)

monotoneBothN :: Int -> XMonotone () (V2 Float)
monotoneBothN n = unindexedXMonotone (V2 (fromIntegral n + 1) 3) (fromJust $ nonEmpty v) (V2 0 3)
  where
    v = fmap (\p -> let V2 _ y = p in if y > 3 then Up p else Down p) $ zipWith V2
        (fmap (\i -> fromIntegral (n - i + 1)) [1 .. n])
        (fmap (\f -> fromIntegral ((f `mod` 6) + 1)) fibs)

monotoneBoth128 :: XMonotone () (V2 Float)
monotoneBoth128 = force (monotoneBothN 128)

monotoneBoth256 :: XMonotone () (V2 Float)
monotoneBoth256 = force (monotoneBothN 256)

monotoneBoth512 :: XMonotone () (V2 Float)
monotoneBoth512 = force (monotoneBothN 512)


polygonSmall :: [(Int, V2 Float)]
polygonSmall = [(0, V2 0 0), (1, V2 1 0), (2, V2 0 1)]

polygonN :: Int -> [(Int, V2 Float)]
polygonN n = fmap func [1 .. n]
  where 
    func i = (i, angle (2 * pi * fromIntegral i / fromIntegral n))

polygon128 :: [(Int, V2 Float)]
polygon128 = force (polygonN 128)

polygon256 :: [(Int, V2 Float)]
polygon256 = force (polygonN 256)

polygon512 :: [(Int, V2 Float)]
polygon512 = force (polygonN 512)

spikeN :: Int -> [(Int, V2 Float)]
spikeN n = fmap func [1 .. n]
  where 
    func i = let s = if even i then 0.5 else 1.0 in (i, s *^ angle (2 * pi * fromIntegral i / fromIntegral n))

spike128 :: [(Int, V2 Float)]
spike128 = force (spikeN 128)

spike256 :: [(Int, V2 Float)]
spike256 = force (spikeN 256)

spike512 :: [(Int, V2 Float)]
spike512 = force (spikeN 512)


polygonToTriangles :: [(Int, V2 Float)] -> [Triangle Int Float]
polygonToTriangles poly = fold ((N.toList . triangulateXMono) <$> monotoneDecomp poly)

benchGeom :: Benchmark
benchGeom = bgroup "Geom"
  [ bgroup "Triangulate"
      [ bench "Small" $ nf triangulateXMono monotoneSmall
      , bench "Top Only 128" $ nf triangulateXMono monotoneTopOnly128
      , bench "Top Only 256" $ nf triangulateXMono monotoneTopOnly256
      , bench "Top Only 512" $ nf triangulateXMono monotoneTopOnly512
      , bench "Both 128" $ nf triangulateXMono monotoneBoth128
      , bench "Both 256" $ nf triangulateXMono monotoneBoth256
      , bench "Both 512" $ nf triangulateXMono monotoneBoth512
      ]
  , bgroup "Make Sorted Marks"
      [ bench "Small" $ nf makeSortedMarks polygonSmall
      , bench "Polygon 128" $ nf makeSortedMarks polygon128
      , bench "Polygon 256" $ nf makeSortedMarks polygon256
      , bench "Polygon 512" $ nf makeSortedMarks polygon512
      , bench "Spike 128" $ nf makeSortedMarks spike128
      , bench "Spike 256" $ nf makeSortedMarks spike256
      , bench "Spike 512" $ nf makeSortedMarks spike512
      ]
  , bgroup "Monotone Decomp"
      [ bench "Small" $ nf monotoneDecomp polygonSmall
      , bench "Polygon 128" $ nf monotoneDecomp polygon128
      , bench "Polygon 256" $ nf monotoneDecomp polygon256
      , bench "Polygon 512" $ nf monotoneDecomp polygon512
      , bench "Spike 128" $ nf monotoneDecomp spike128
      , bench "Spike 256" $ nf monotoneDecomp spike256
      , bench "Spike 512" $ nf monotoneDecomp spike512
      ]
  , bgroup "Polygon to Triangle"
      [ bench "Small" $ nf polygonToTriangles polygonSmall
      , bench "Polygon 128" $ nf polygonToTriangles polygon128
      , bench "Polygon 256" $ nf polygonToTriangles polygon256
      , bench "Polygon 512" $ nf polygonToTriangles polygon512
      , bench "Spike 128" $ nf polygonToTriangles spike128
      , bench "Spike 256" $ nf polygonToTriangles spike256
      , bench "Spike 512" $ nf polygonToTriangles spike512
      ]
  ]