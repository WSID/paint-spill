module Test.Geom.Util where

import Data.Bool

import Linear


-- Makes grid membership distribution
makeCoordList :: V2 Float -> V2 Float -> V2 Float -> [[V2 Float]]
makeCoordList (V2 sx sy) (V2 nx ny) (V2 ex ey) =
    (\y -> (`V2` y) <$> [sx, nx .. ex]) <$> reverse [sy, ny .. ey]

coordList :: [[V2 Float]]
coordList = makeCoordList (V2 (-4) (-4)) (V2 (-3.75) (-3.75)) (V2 4 4)

mapCoordList :: (a -> b) -> [[a]] -> [[b]]
mapCoordList f a = fmap f <$> a

zipCoordList :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipCoordList f = zipWith (zipWith f)

newtype CoverMap = CoverMap [[Bool]] deriving Eq
instance Show CoverMap where
    show (CoverMap a) = "\n" ++ (unlines $ fmap (fmap $ bool ' ' '*') a) 