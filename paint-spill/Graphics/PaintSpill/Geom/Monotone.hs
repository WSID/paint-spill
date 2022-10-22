module Graphics.PaintSpill.Geom.Monotone where

import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as N

import Control.DeepSeq

import Linear

import Graphics.PaintSpill.Util
import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Geom.Triangle


data XMonotone i a = XMonotone
  { xmonoEnd :: (i, V2 a)
  , xmonoStrip :: NonEmpty (DownUp (i, V2 a))
  , xmonoStart :: (i, V2 a)
  }
  deriving Show

instance (NFData i, NFData a) => NFData (XMonotone i a) where
    rnf (XMonotone e m s) = rnf e `seq` rnf m `seq` rnf s

-- | Get Y range of a x monotone.
xmonoY :: (Ord a, Fractional a) => XMonotone i a -> a -> (a, a)
xmonoY (XMonotone (_, e) m (_, s)) x = (xstripYDown e d s x, xstripYUp e u s x)
  where
    (di, ui) = splitList m
    d = fmap snd di
    u = fmap snd ui

-- | Membership check for x monotone, as shape.
xmonoElem :: (Ord a, Fractional a) => XMonotone i a -> V2 a -> Bool
xmonoElem mt (V2 x y) = (sx <= x) && (x <= ex) && (l <= y) && (y <= h)
  where
    XMonotone (_, me) _ (_, ms) = mt
    V2 sx _ = ms
    V2 ex _ = me
    (l, h) = xmonoY mt x

-- | Unindexed Monotone
unindexedXMonotone :: V2 a -> NonEmpty (DownUp (V2 a)) -> V2 a -> XMonotone () a
unindexedXMonotone e m s = XMonotone ((), e) (fmap (\v -> ((), v)) <$> m) ((), s)


-- | Triangulate Monotone

triangulateXMono :: (Ord a, Fractional a) => XMonotone i a -> NonEmpty (Triangle i a)
triangulateXMono m = triangulateXMonoGo m []


triangulateXMonoDone :: (i, V2 a) -> DownUp (i, V2 a) -> (i, V2 a) -> [Triangle i a] -> NonEmpty (Triangle i a)
triangulateXMonoDone e (Down d) s accum = Triangle s d e :| accum
triangulateXMonoDone e (Up u) s accum = Triangle s e u :| accum


triangulateXMonoGo :: (Ord a, Fractional a) => XMonotone i a -> [Triangle i a] -> NonEmpty (Triangle i a)
triangulateXMonoGo (XMonotone e m s) accum = case N.uncons m of
  (ma,      Nothing) -> triangulateXMonoDone e ma s accum
  (Down d,  Just mn) -> triangulateXMonoDown (d :| [e]) (XMonotone e mn s) accum 
  (Up u,    Just mn) -> triangulateXMonoUp (u :| [e]) (XMonotone e mn s) accum

triangulateXMonoUp :: (Ord a, Fractional a) => NonEmpty (i, V2 a) -> XMonotone i a -> [Triangle i a] -> NonEmpty (Triangle i a)
triangulateXMonoUp st (XMonotone e m s) accum = case ma of
  Down d -> 
      let sth :| _ = st
          naccum = stackFlushUp d st accum
      in maybe
          (triangulateXMonoDone sth ma s naccum)
          (\mn -> triangulateXMonoDown (d :| [sth]) (XMonotone sth mn s) naccum)
          ms
  Up u -> 
      let (naccum, nst) = stackWindUp (u <| st) accum
      in maybe
        (triangulateXMonoDone e ma s (stackFlushUp u nst naccum))
        (\mn -> triangulateXMonoUp nst (XMonotone e mn s) naccum)
        ms 
  where
    (ma, ms) = N.uncons m

triangulateXMonoDown :: (Ord a, Fractional a) => NonEmpty (i, V2 a) -> XMonotone i a -> [Triangle i a] -> NonEmpty (Triangle i a)
triangulateXMonoDown st (XMonotone e m s) accum  = case ma of
  Down d -> 
      let (naccum, nst) = stackWindDown (d <| st) accum
      in maybe
          (triangulateXMonoDone e ma s (stackFlushDown d nst naccum))
          (\mn -> triangulateXMonoDown nst (XMonotone e mn s) naccum)
          ms
  Up u ->
      let sth :| _ = st
          naccum = stackFlushDown u st accum
      in maybe
          (triangulateXMonoDone sth ma s naccum)
          (\mn -> triangulateXMonoUp (u :| [sth]) (XMonotone sth mn s) naccum)
          ms
  where
    (ma, ms) = N.uncons m


stackWindUp :: (Ord a, Fractional a) => NonEmpty (i, V2 a) -> [Triangle i a] -> ([Triangle i a], NonEmpty (i, V2 a))
stackWindUp (a :| b : c : d) accum
  | isConcave = (accum, a :| b : c : d)
  | otherwise = let (naccum, nt) = stackWindUp (a :| c : d) accum in (Triangle b a c: naccum, nt)
  where
    bv = snd b
    cv = snd c
    V2 ax ay = snd a
    V2 bx by = bv
    V2 cx cy = cv
    isConcave = if bx == cx then by < cy else segYUp bv cv ax < ay 
stackWindUp s accum = (accum, s)

stackWindDown :: (Ord a, Fractional a) => NonEmpty (i, V2 a) -> [Triangle i a] -> ([Triangle i a], NonEmpty (i, V2 a))
stackWindDown (a :| b : c : d) accum
  | isConcave = (accum, a :| b : c : d)
  | otherwise = let (naccum, nt) = stackWindDown (a :| c : d) accum in (Triangle b c a: naccum, nt)
  where
    bv = snd b
    cv = snd c
    V2 ax ay = snd a
    V2 bx by = bv
    V2 cx cy = cv
    isConcave = if bx == cx then by > cy else segYDown bv cv ax > ay 
stackWindDown s accum = (accum, s)

stackFlushUp :: (i, V2 a) -> NonEmpty (i, V2 a) -> [Triangle i a] -> [Triangle i a]
stackFlushUp da (a :| b : c) accum = Triangle da b a : stackFlushUp da (b :| c) accum
stackFlushUp _ (_ :| []) accum = accum

stackFlushDown :: (i, V2 a) -> NonEmpty (i, V2 a) -> [Triangle i a] -> [Triangle i a]
stackFlushDown ua (a :| b : c) accum = Triangle ua a b : stackFlushUp ua (b :| c) accum
stackFlushDown _ (_ :| []) accum = accum
