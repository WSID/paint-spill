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


segIntersect :: (Ord a, Fractional a) => V2 a -> V2 a -> V2 a -> V2 a -> Maybe (V2 a)
segIntersect aa ab ba bb
  | aax == abx  = if inRange bax bbx aax then let ty = segY ba bb aax in if inRange aay aby ty then Just (V2 aax ty) else Nothing else Nothing
  | bax == bbx  = if inRange aax abx bax then let ty = segY aa ab bax in if inRange bay bby ty then Just (V2 bax ty) else Nothing else Nothing 
  | mdiff == 0  = Nothing
  | otherwise   = Just (V2 (bax + dx) (bay + dx * bm))
  where
    V2 aax aay = aa
    V2 abx aby = ab
    V2 bax bay = ba
    V2 bbx bby = bb
    V2 adx ady = ab - aa
    V2 bdx bdy = bb - ba
    am = ady / adx
    bm = bdy / bdx
    mdiff = am - bm
    height = bay - aay - (bax - aax) * am
    dx = height / mdiff

    inRange a b c
      | a < b     = (a <= c) && (c <= b)
      | otherwise = (b <= c) && (c <= a)


data SegCache a = SegCache a a | SegVertical a

makeSegCache :: (Ord a, Fractional a) => V2 a -> V2 a -> SegCache a
makeSegCache a b
  | dx == 0   = SegVertical ax
  | otherwise = SegCache m (ay - ax * m)
  where
    V2 dx dy = b - a
    V2 ax ay = a
    m = dy / dx

segCacheY :: (Num a) => SegCache a -> a -> Maybe a
segCacheY (SegCache m c) x = Just (c + m * x)
segCacheY (SegVertical _) _ = Nothing

segCacheIntersect :: (Eq a, Fractional a) => SegCache a -> SegCache a -> Maybe (V2 a)
segCacheIntersect (SegVertical _) (SegVertical _) = Nothing
segCacheIntersect (SegVertical x) (SegCache m c) = Just (V2 x (c + m * x))
segCacheIntersect (SegCache m c) (SegVertical x) = Just (V2 x (c + m * x))
segCacheIntersect (SegCache am ac) (SegCache bm bc) 
  | dm == 0   = Nothing
  | otherwise = Just (V2 cx cy)
  where
    dm = bm - am
    cx = (ac - bc) / dm
    cy = ac + am * cx