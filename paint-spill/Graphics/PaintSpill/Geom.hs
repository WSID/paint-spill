{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Graphics.PaintSpill.Geom where

import Data.Maybe
import Linear

-- | Get y coordinate of a segment.
segYUp :: (Ord a, Fractional a) => V2 a -> V2 a -> a -> a
segYUp a b x
  | ax == bx  = max ay by
  | otherwise = (by - ay) / (bx - ax) * (x - ax) + ay
  where
    V2 ax ay = a
    V2 bx by = b

segYDown :: (Ord a, Fractional a) => V2 a -> V2 a -> a -> a
segYDown a b x
  | ax == bx  = min ay by
  | otherwise = (by - ay) / (bx - ax) * (x - ax) + ay
  where
    V2 ax ay = a
    V2 bx by = b

-- | Get y coordinate of x-aligned strip
xstripYUp :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> a -> a
xstripYUp end strip start x
  | ex <= x     = ey
  | x <= sx     = sy
  | otherwise   = go end strip start
  where
    V2 ex ey = end
    V2 sx sy = start
    go a [] s = segYUp s a x
    go a (b : sn) s =
        let V2 ax ay = a
            V2 bx by = b
        in  case compare bx x of
                LT -> segYUp b a x
                EQ -> if ax == x then segYUp b a x else go b sn s
                _ -> go b sn s

xstripYDown :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> a -> a
xstripYDown end strip start x
  | ex <= x     = ey
  | x <= sx     = sy
  | otherwise   = go end strip start
  where
    V2 ex ey = end
    V2 sx sy = start
    go a [] s = segYDown s a x
    go a (b : sn) s =
        let V2 ax ay = a
            V2 bx by = b
        in  case compare bx x of
                LT -> segYDown b a x
                EQ -> if ax == x then segYDown b a x else go b sn s
                _ -> go b sn s

-- | A Triangle type.
-- 
-- Triangle is just a bunch of 3 data.
--
data Triangle a = Triangle a a a deriving (Eq, Functor, Foldable)

-- | Membership check for triangle, as shape.
triElem :: (Ord a, Fractional a) => Triangle (V2 a) -> V2 a -> Bool
triElem (Triangle a b c) e = (0 <= i) && (0 <= j) && (i + j <= 1)
  where
    ab = b - a
    ac = c - a
    ae = e - a
    m = V2 ab ac
    V2 i j = ae *! inv22 m

data XMonotone a = XMonotone a [a] [a] a

-- | Get Y range of a x monotone.
xmonoY :: (Ord a, Fractional a) => XMonotone (V2 a) -> a -> (a, a)
xmonoY (XMonotone e u d s) x = (xstripYDown e d s x, xstripYUp e u s x)

-- | Membership check for x monotone, as shape.
xmonoElem :: (Ord a, Fractional a) => XMonotone (V2 a) -> V2 a -> Bool
xmonoElem mt (V2 x y) = (l <= y) && (y <= h)
  where
    (l, h) = xmonoY mt x