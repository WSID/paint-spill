{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.PaintSpill.Geom where

import Linear

-- | Get y coord of a point with x, on segment.
--
-- Assumes the segment is not vertical. If divide-by-zero is concern, then
-- segYUp or segYDown can be used.
segY :: (Ord a, Fractional a) => V2 a -> V2 a -> a -> a
segY (V2 ax ay) (V2 bx by) x = (by - ay) / (bx - ax) * (x - ax) + ay

-- | Get y coordinate of a segment.
--
-- On vertical segment, up point is returned.
segYUp :: (Ord a, Fractional a) => V2 a -> V2 a -> a -> a
segYUp a b x
  | ax == bx  = max ay by
  | otherwise = segY a b x
  where
    V2 ax ay = a
    V2 bx by = b

-- | Get y coordinate of a segment.
--
-- On vertical segment, down point is returned.
segYDown :: (Ord a, Fractional a) => V2 a -> V2 a -> a -> a
segYDown a b x
  | ax == bx  = min ay by
  | otherwise = segY a b x
  where
    V2 ax ay = a
    V2 bx by = b


-- | Get y coordinate of x-aligned strip
xstripYUp :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> a -> a
xstripYUp end strip start x
  | ex < x     = ey
  | x < sx     = sy
  | otherwise  = go end strip start
  where
    V2 ex ey = end
    V2 sx sy = start
    go a [] s = segYUp s a x
    go a (b : sn) s =
        let V2 ax ay = a
            V2 bx by = b
        in  case compare bx x of
                LT -> segY b a x
                EQ -> if ax == x then max ay by else go b sn s
                _ -> go b sn s

xstripYDown :: (Ord a, Fractional a) => V2 a -> [V2 a] -> V2 a -> a -> a
xstripYDown end strip start x
  | ex < x     = ey
  | x < sx     = sy
  | otherwise  = go end strip start
  where
    V2 ex ey = end
    V2 sx sy = start
    go a [] s = segYDown s a x
    go a (b : sn) s =
        let V2 ax ay = a
            V2 bx by = b
        in  case compare bx x of
                LT -> segY b a x
                EQ -> if ax == x then min ay by else go b sn s
                _ -> go b sn s
