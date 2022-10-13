{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Graphics.PaintSpill.Geom where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as N
import Data.Maybe

import Control.DeepSeq
import Linear

import Graphics.PaintSpill.Util

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

-- | A Triangle type.
-- 
-- Triangle is just a bunch of 3 data.
--
data Triangle a = Triangle a a a deriving (Eq, Functor, Foldable)

instance NFData a => NFData (Triangle a) where
    rnf (Triangle a b c) = rnf a `seq` rnf b `seq` rnf c

-- | Membership check for triangle, as shape.
triElem :: (Ord a, Fractional a) => Triangle (V2 a) -> V2 a -> Bool
triElem (Triangle a b c) e = (0 <= i) && (0 <= j) && (i + j <= 1)
  where
    ab = b - a
    ac = c - a
    ae = e - a
    m = V2 ab ac
    V2 i j = ae *! inv22 m

data XMonotone a = XMonotone a (NonEmpty (DownUp a)) a

instance NFData a => NFData (XMonotone a) where
    rnf (XMonotone e m s) = rnf e `seq` rnf m `seq` rnf s

-- | Get Y range of a x monotone.
xmonoY :: (Ord a, Fractional a) => XMonotone (V2 a) -> a -> (a, a)
xmonoY (XMonotone e m s) x = (xstripYDown e d s x, xstripYUp e u s x)
  where
    (d, u) = splitList m

-- | Membership check for x monotone, as shape.
xmonoElem :: (Ord a, Fractional a) => XMonotone (V2 a) -> V2 a -> Bool
xmonoElem mt (V2 x y) = (sx <= x) && (x <= ex) && (l <= y) && (y <= h)
  where
    XMonotone me _ ms = mt
    V2 sx _ = ms
    V2 ex _ = me
    (l, h) = xmonoY mt x


-- | Triangulate Monotone

triangulateXMono :: (Ord a, Fractional a) => XMonotone (V2 a) -> [Triangle (V2 a)]
triangulateXMono m = triangulateXMonoGo m []


triangulateXMonoDone :: V2 a -> DownUp (V2 a) -> V2 a -> [Triangle (V2 a)] -> [Triangle (V2 a)]
triangulateXMonoDone e (Down d) s accum = Triangle s d e : accum
triangulateXMonoDone e (Up u) s accum = Triangle s e u : accum


triangulateXMonoGo :: (Ord a, Fractional a) => XMonotone (V2 a) -> [Triangle (V2 a)] -> [Triangle (V2 a)]
triangulateXMonoGo (XMonotone e m s) accum = case N.uncons m of
  (ma,      Nothing) -> triangulateXMonoDone e ma s accum
  (Down d,  Just mn) -> triangulateXMonoDown (d :| [e]) (XMonotone e mn s) accum 
  (Up u,    Just mn) -> triangulateXMonoUp (u :| [e]) (XMonotone e mn s) accum

triangulateXMonoUp :: (Ord a, Fractional a) => NonEmpty (V2 a) -> XMonotone (V2 a) -> [Triangle (V2 a)] -> [Triangle (V2 a)]
triangulateXMonoUp st (XMonotone e m s) accum = case ma of
  Down d -> 
      let sth :| _ = st
      in triangulateXMonoGo (XMonotone sth m s) (stackFlushUp d st accum)
  Up u -> 
      let (naccum, nst) = stackWindUp (u <| st) accum
      in maybe
        (triangulateXMonoDone e ma s (stackFlushUp u nst naccum))
        (\mn -> triangulateXMonoUp nst (XMonotone e mn s) naccum)
        ms 
  where
    (ma, ms) = N.uncons m

triangulateXMonoDown :: (Ord a, Fractional a) => NonEmpty (V2 a) -> XMonotone (V2 a) -> [Triangle (V2 a)] -> [Triangle (V2 a)]
triangulateXMonoDown st (XMonotone e m s) accum  = case ma of
  Down d -> 
      let (naccum, nst) = stackWindDown (d <| st) accum
      in maybe
          (triangulateXMonoDone e ma s (stackFlushDown d nst naccum))
          (\mn -> triangulateXMonoDown nst (XMonotone e mn s) naccum)
          ms
  Up u ->
      let sth :| _ = st
      in triangulateXMonoGo (XMonotone sth m s) (stackFlushDown u st accum)
  where
    (ma, ms) = N.uncons m


stackWindUp :: (Ord a, Fractional a) => NonEmpty (V2 a) -> [Triangle (V2 a)] -> ([Triangle (V2 a)], NonEmpty (V2 a))
stackWindUp (a :| b : c : d) accum
  | isConcave = (accum, a :| b : c : d)
  | otherwise = let (naccum, nt) = stackWindUp (a :| c : d) accum in (Triangle b a c: naccum, nt)
  where
    V2 ax ay = a
    V2 bx by = b
    V2 cx cy = c
    isConcave = if bx == cx then by < cy else segYUp b c ax < ay 
stackWindUp s accum = (accum, s)

stackWindDown :: (Ord a, Fractional a) => NonEmpty (V2 a) -> [Triangle (V2 a)] -> ([Triangle (V2 a)], NonEmpty (V2 a))
stackWindDown (a :| b : c : d) accum
  | isConcave = (accum, a :| b : c : d)
  | otherwise = let (naccum, nt) = stackWindDown (a :| c : d) accum in (Triangle b c a: naccum, nt)
  where
    V2 ax ay = a
    V2 bx by = b
    V2 cx cy = c
    isConcave = if bx == cx then by > cy else segYDown b c ax > ay 
stackWindDown s accum = (accum, s)

stackFlushUp :: V2 a -> NonEmpty (V2 a) -> [Triangle (V2 a)] -> [Triangle (V2 a)]
stackFlushUp da (a :| b : c) accum = Triangle da b a : stackFlushUp da (b :| c) accum
stackFlushUp _ (_ :| []) accum = accum

stackFlushDown :: V2 a -> NonEmpty (V2 a) -> [Triangle (V2 a)] -> [Triangle (V2 a)]
stackFlushDown ua (a :| b : c) accum = Triangle ua a b : stackFlushUp ua (b :| c) accum
stackFlushDown _ (_ :| []) accum = accum


-- Polygons

polyElem :: (Ord a, Fractional a) => V2 a -> [V2 a] -> Bool
polyElem e p = go e (polyToSegs p) 0
  where
    polyToSegs vs = zip vs (tail . cycle $ vs)

    onVertical y a b
      | a < b     = (a <= y) && (y <= b)
      | otherwise = (b <= y) && (y <= a)

    go v ((a, b) : s) n
      | v == b    = True
      | ax < bx   = go v s $ if (ax <= x) && (x < bx) && (segY a b x <= y) then n + 1 else n
      | bx < ax   = go v s $ if (bx <= x) && (x < ax) && (segY a b x < y) then n + 1 else n
      | ax == x   = onVertical y ay by || go v s n
      | otherwise = go v s n
      where
        V2 x y = v
        V2 ax ay = a
        V2 bx by = b
    go _ [] n = odd n
