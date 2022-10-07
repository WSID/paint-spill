{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Graphics.PaintSpill.Geom where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as N
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
  | ex < x     = ey
  | x < sx     = sy
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
  | ex < x     = ey
  | x < sx     = sy
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
xmonoElem mt (V2 x y) = (sx <= x) && (x <= ex) && (l <= y) && (y <= h)
  where
    XMonotone me _ _ ms = mt
    V2 sx _ = ms
    V2 ex _ = me
    (l, h) = xmonoY mt x


-- | Triangulate Monotone
triangulateXMono :: (Ord a, Fractional a) => XMonotone (V2 a) -> [Triangle (V2 a)]
triangulateXMono (XMonotone e (ua : u) (da : d) s)
  | dax <= uax  = triangulateXMonoUp (ua :| [e]) (XMonotone e u (da : d) s)
  | otherwise   = triangulateXMonoDown (da :| [e]) (XMonotone e (ua : u) d s)
  where
    V2 uax _ = ua
    V2 dax _ = da
triangulateXMono (XMonotone e (ua : u) [] s) = triangulateXMonoUpOnly (ua :| [e]) s u 
triangulateXMono (XMonotone e [] (da : d) s) = triangulateXMonoDownOnly (da :| [e]) s d
triangulateXMono _ = []


triangulateXMonoUp :: (Ord a, Fractional a) => NonEmpty (V2 a) -> XMonotone (V2 a) -> [Triangle (V2 a)]
triangulateXMonoUp st (XMonotone e (ua : u) (da : d) s)
  | dax <= uax  =
      let (r, nst) = stackWindUp (ua <| st)
      in triangulateXMonoUp nst (XMonotone e u (da : d) s) ++ r
  | otherwise   =
      let sth :| _ = st
      in triangulateXMono (XMonotone sth (ua : u) (da : d) s) ++ stackFlushUp da st
  where
    V2 uax _ = ua
    V2 dax _ = da
triangulateXMonoUp st (XMonotone e [] (da : d) s) =
  let sth :| _ = st
  in triangulateXMonoDownOnly (da :| [sth]) s d ++ stackFlushUp da st
triangulateXMonoUp _ mt = error "Not happening!"

triangulateXMonoDown :: (Ord a, Fractional a) => NonEmpty (V2 a) -> XMonotone (V2 a) -> [Triangle (V2 a)]
triangulateXMonoDown st (XMonotone e (ua : u) (da : d) s)
  | uax < dax   =
      let (r, nst) = stackWindDown (da <| st)
      in triangulateXMonoDown nst (XMonotone e (ua : u) d s) ++ r
  | otherwise   =
      let sth :| _ = st
      in triangulateXMono (XMonotone sth (ua : u) (da : d) s) ++ stackFlushDown ua st
  where
    V2 uax _ = ua
    V2 dax _ = da
triangulateXMonoDown st (XMonotone e (ua : u) [] s) =
    let sth :| _ = st
    in triangulateXMonoUpOnly (ua :| [sth]) s u ++ stackFlushDown ua st

triangulateXMonoDown _ mt = error "Not happening!"


triangulateXMonoUpOnly :: (Ord a, Fractional a) => NonEmpty (V2 a) -> V2 a -> [V2 a] -> [Triangle (V2 a)]
triangulateXMonoUpOnly st s (ua : u) =
    let (r, nst) = stackWindUp (ua <| st)
    in triangulateXMonoUpOnly nst s u ++ r
triangulateXMonoUpOnly st s [] = stackFlushUp s st

triangulateXMonoDownOnly :: (Ord a, Fractional a) => NonEmpty (V2 a) -> V2 a -> [V2 a] -> [Triangle (V2 a)]
triangulateXMonoDownOnly st s (ua : u) =
    let (r, nst) = stackWindDown (ua <| st)
    in triangulateXMonoDownOnly nst s u ++ r
triangulateXMonoDownOnly st s [] = stackFlushDown s st


stackWindUp :: (Ord a, Fractional a) => NonEmpty (V2 a) -> ([Triangle (V2 a)], NonEmpty (V2 a))
stackWindUp (a :| b : c : d)
  | isConcave = ([], a :| b : c : d)
  | otherwise = stackWindUp (a :| c : d) <* ([Triangle b a c], ())
  where
    V2 ax ay = a
    V2 bx by = b
    V2 cx cy = c
    isConcave = if bx == cx then by < cy else segYUp b c ax < ay 
stackWindUp s = ([], s)

stackWindDown :: (Ord a, Fractional a) => NonEmpty (V2 a) -> ([Triangle (V2 a)], NonEmpty (V2 a))
stackWindDown (a :| b : c : d)
  | isConcave = ([], a :| b : c : d)
  | otherwise = stackWindDown (a :| c : d) <* ([Triangle b c a], ())
  where
    V2 ax ay = a
    V2 bx by = b
    V2 cx cy = c
    isConcave = if bx == cx then by > cy else segYDown b c ax > ay 
stackWindDown s = ([], s)

stackFlushUp :: V2 a -> NonEmpty (V2 a) -> [Triangle (V2 a)]
stackFlushUp da (a :| b : c) = Triangle da b a : stackFlushUp da (b :| c)
stackFlushUp _ (_ :| []) = []

stackFlushDown :: V2 a -> NonEmpty (V2 a) -> [Triangle (V2 a)]
stackFlushDown ua (a :| b : c) = Triangle ua a b : stackFlushUp ua (b :| c)
stackFlushDown _ (_ :| []) = []