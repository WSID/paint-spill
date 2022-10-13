module Bench.Geom where

import Data.Maybe
import Data.List.NonEmpty (nonEmpty, NonEmpty ((:|)))

import Control.DeepSeq
import Gauge

import Linear
import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Util

-- A fibonacci list to make numeric patterns.
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

monotoneSmall :: XMonotone (V2 Float)
monotoneSmall = force (XMonotone (V2 1 0) (Up (V2 0 1) :| []) (V2 0 0))

monotoneTopOnlyN :: Int -> XMonotone (V2 Float)
monotoneTopOnlyN n = XMonotone (V2 (fromIntegral n + 1) 0) (fromJust $ nonEmpty u) (V2 0 0)
  where
    u = fmap Up $ zipWith V2
        (fmap (\i -> fromIntegral (n - i + 1)) [1 .. n])
        (fmap (\f -> fromIntegral ((f `mod` 5) + 1)) fibs)

monotoneTopOnly128 :: XMonotone (V2 Float)
monotoneTopOnly128 = force (monotoneTopOnlyN 128)

monotoneTopOnly256 :: XMonotone (V2 Float)
monotoneTopOnly256 = force (monotoneTopOnlyN 256)

monotoneTopOnly512 :: XMonotone (V2 Float)
monotoneTopOnly512 = force (monotoneTopOnlyN 512)

monotoneBothN :: Int -> XMonotone (V2 Float)
monotoneBothN n = XMonotone (V2 (fromIntegral n + 1) 3) (fromJust $ nonEmpty v) (V2 0 3)
  where
    v = fmap (\p -> let V2 _ y = p in if y > 3 then Up p else Down p) $ zipWith V2
        (fmap (\i -> fromIntegral (n - i + 1)) [1 .. n])
        (fmap (\f -> fromIntegral ((f `mod` 6) + 1)) fibs)

monotoneBoth128 :: XMonotone (V2 Float)
monotoneBoth128 = force (monotoneBothN 128)

monotoneBoth256 :: XMonotone (V2 Float)
monotoneBoth256 = force (monotoneBothN 256)

monotoneBoth512 :: XMonotone (V2 Float)
monotoneBoth512 = force (monotoneBothN 512)

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
  ]