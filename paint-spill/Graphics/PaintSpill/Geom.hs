{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.PaintSpill.Geom where

import Data.Semigroup
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as N
import Data.Maybe

import Control.DeepSeq
import Linear

import Graphics.PaintSpill.Util
import Data.Function (on)

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

data XMonotone a = XMonotone a (NonEmpty (DownUp a)) a deriving Show

instance Functor XMonotone where
    fmap :: (a -> b) -> XMonotone a -> XMonotone b
    fmap f (XMonotone e m s) = XMonotone (f e) (fmap (fmap f) m) (f s)

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

triangulateXMono :: (Ord a, Fractional a) => XMonotone (V2 a) -> NonEmpty (Triangle (V2 a))
triangulateXMono m = triangulateXMonoGo m []


triangulateXMonoDone :: V2 a -> DownUp (V2 a) -> V2 a -> [Triangle (V2 a)] -> NonEmpty (Triangle (V2 a))
triangulateXMonoDone e (Down d) s accum = Triangle s d e :| accum
triangulateXMonoDone e (Up u) s accum = Triangle s e u :| accum


triangulateXMonoGo :: (Ord a, Fractional a) => XMonotone (V2 a) -> [Triangle (V2 a)] -> NonEmpty (Triangle (V2 a))
triangulateXMonoGo (XMonotone e m s) accum = case N.uncons m of
  (ma,      Nothing) -> triangulateXMonoDone e ma s accum
  (Down d,  Just mn) -> triangulateXMonoDown (d :| [e]) (XMonotone e mn s) accum 
  (Up u,    Just mn) -> triangulateXMonoUp (u :| [e]) (XMonotone e mn s) accum

triangulateXMonoUp :: (Ord a, Fractional a) => NonEmpty (V2 a) -> XMonotone (V2 a) -> [Triangle (V2 a)] -> NonEmpty (Triangle (V2 a))
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

triangulateXMonoDown :: (Ord a, Fractional a) => NonEmpty (V2 a) -> XMonotone (V2 a) -> [Triangle (V2 a)] -> NonEmpty (Triangle (V2 a))
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
polyElem e p = go e segs 0
  where
    segs = zip p (tail . cycle $ p)

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


data MonoMark a
    = MonoLeft a a
    | MonoRight a a
    | MonoStart a a a 
    | MonoFork a a a
    | MonoJoin a
    | MonoEnd a
    deriving (Show)

data MonoTrack a
    = MonoTSingle a a a [DownUp a]
    | MonoTFork a a a (NonEmpty (DownUp a)) a (NonEmpty (DownUp a))

makeSortedMarks :: (Ord a, Fractional a) => [(i, V2 a)] -> [MonoMark (i, V2 a)]
makeSortedMarks poly = marks
  where
    cmpVerts (_, V2 ax ay) (_, V2 bx by) = if xcmp == EQ then compare ay by else xcmp
      where
        xcmp = compare ax bx
    
    makeMonoMark (LT, a, b) (LT, _, c) = (b, MonoLeft b c)
    makeMonoMark (GT, a, b) (GT, _, c) = (b, MonoRight b a)
    makeMonoMark (LT, a, b) (GT, _, c)
      | ay < cy   = (b, MonoEnd b)
      | otherwise = (b, MonoJoin b)
      where
        (_, V2 _ ay) = a
        (_, V2 _ cy) = c

    makeMonoMark (GT, a, b) (LT, _, c)
      | ay < cy   = (b, MonoFork a b c)
      | otherwise = (b, MonoStart a b c)
      where
        (_, V2 _ ay) = a
        (_, V2 _ cy) = c

    makeMonoMark _ _ = error "Not happening!"

    segs = zipWith (\a b -> (cmpVerts a b, a, b)) poly (tail . cycle $ poly)
    marksNotSorted = zipWith makeMonoMark segs (tail . cycle $ segs)
    marks = snd <$> sortBy (cmpVerts `on` fst) marksNotSorted


monotoneDecomp :: (Eq i, Ord a, Fractional a) => [(i, V2 a)] -> [XMonotone (i, V2 a)]
monotoneDecomp poly = monotoneDecompGo (makeSortedMarks poly) [] []


monotoneDecompGo :: (Eq i, Ord a) => [MonoMark (i, V2 a)] -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> [XMonotone (i, V2 a)]
monotoneDecompGo [] [] accum = accum
monotoneDecompGo [] _ accum = error "Not concluded tracks."
monotoneDecompGo (m : ms) tracks accum = case m of
  MonoStart a b c -> monotoneDecompGo ms (MonoTSingle c a b []: tracks) accum

  MonoLeft b c -> let (ntracks, naccum) = monotoneDecompLeft b c tracks accum in monotoneDecompGo ms ntracks naccum
  MonoRight b a -> let (ntracks, naccum) = monotoneDecompRight b a tracks accum in monotoneDecompGo ms ntracks naccum
  MonoEnd b -> let (ntracks, naccum) = monotoneDecompEnd b tracks accum in monotoneDecompGo ms ntracks naccum
  MonoFork a b c -> let (ntracks, naccum) = monotoneDecompFork a b c tracks accum in monotoneDecompGo ms ntracks naccum
  MonoJoin a -> let (ntracks, naccum) = monotoneDecompJoin a tracks accum in monotoneDecompGo ms ntracks naccum
  

monotoneDecompLeft :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompLeft (ai, av) b (t: ts) accum = case t of

  MonoTSingle (di, dv) u s st -> if ai == di
    then (MonoTSingle b u s (Down (di, dv) : st): ts, accum)
    else let (nts, naccum) = monotoneDecompLeft (ai, av) b ts accum in (t: nts, naccum)

  MonoTFork (di, dv) u ds dst us ust -> if ai == di
    then
        ( MonoTSingle b u us (Down (di, dv) : N.toList ust) : ts
        , XMonotone (di, dv) dst ds : accum)
    else let (nts, naccum) = monotoneDecompLeft (ai, av) b ts accum in (t: nts, naccum)

monotoneDecompLeft _ _ [] _ = error "Matching not found!"




monotoneDecompRight :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompRight (ai, av) b (t: ts) accum = case t of

  MonoTSingle d (ui, uv) s st -> if ai == ui
    then (MonoTSingle d b s (Up (ui, uv) : st): ts, accum)
    else let (nts, naccum) = monotoneDecompRight (ai, av) b ts accum in (t: nts, naccum)

  MonoTFork d (ui, uv) ds dst us ust -> if ai == ui
    then
        ( MonoTSingle d b ds (Up (ui, uv) : N.toList dst) : ts
        , XMonotone (ui, uv) ust us : accum)
    else let (nts, naccum) = monotoneDecompRight (ai, av) b ts accum in (t: nts, naccum)

monotoneDecompRight _ _ [] _ = error "Matching not found!"



monotoneDecompEnd :: (Eq i, Ord a) => (i, V2 a) -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompEnd (bi, bv) (t: ts) accum = case t of

  MonoTSingle (di, V2 dx dy) (ui, V2 ux uy) s st -> if bi == di
    then if bi == ui
      then case st of
        (sth : stn) -> (ts, XMonotone (bi, bv) (sth :| stn) s : accum)
        [] -> error "A monotone with 2 vertices!"
      else
        error "Partial matching track for end: downside is matching."
    else if bi == ui
      then error "Partial matching track for end: upside is matching."
      else let (nts, naccum) = monotoneDecompEnd (bi, bv) ts accum in (t: nts, naccum)

  MonoTFork (di, V2 dx dy) (ui, V2 ux uy) ds dst us ust -> if bi == di
    then if bi == ui
      then
        ( ts
        , XMonotone (bi, bv) (Down (di, V2 dx dy) <| dst) ds
        : XMonotone (bi, bv) (Up (ui, V2 ux uy) <| ust) us
        : accum)
      else
        error "Partial matching track for end: downside is matching."
    else if bi == ui
      then error "Partial matching track for end: upside is matching."
      else let (nts, naccum) = monotoneDecompEnd (bi, bv) ts accum in (t: nts, naccum)

monotoneDecompEnd _ [] _ = error "Matching track for End not found!"


monotoneDecompFork :: (Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompFork a b c (t: ts) accum = case t of

  MonoTSingle (di, V2 dx dy) (ui, V2 ux uy) s st -> if (dy <= by) && (by <= uy)
    then case st of
      [] ->
        ( MonoTSingle (di, V2 dx dy) a s [Up b]
        : MonoTSingle c (ui, V2 ux uy) s [Down b]
        : ts
        , accum
        )
      (Down hd: nst) ->
        ( MonoTSingle (di, V2 dx dy) a hd [Up b]
        : MonoTSingle c (ui, V2 ux uy) s (Down b : st)
        : ts
        , accum
        )
      (Up hu: nst) ->
        ( MonoTSingle (di, V2 dx dy) a s (Up b : st)
        : MonoTSingle c (ui, V2 ux uy) hu [Down b]
        : ts
        , accum
        )
    else
      let (nts, naccum) = monotoneDecompFork a (bi, V2 bx by)c ts accum in (t: nts, naccum)
  
  MonoTFork (di, V2 dx dy) (ui, V2 ux uy) ds dst us ust -> if (dy <= by) && (by <= uy)
    then
      ( MonoTSingle (di, V2 dx dy) a ds (Up b : N.toList dst)
      : MonoTSingle c (ui, V2 ux uy) us (Down b : N.toList ust)
      : ts
      , accum
      )
    else
      let (nts, naccum) = monotoneDecompFork a (bi, V2 bx by)c ts accum in (t: nts, naccum)
  where
    (bi, V2 bx by) = b

monotoneDecompFork _ _ _ [] _ = error "Suitable track for Fork, not found!"

monotoneDecompJoin :: (Eq i, Ord a) => (i, V2 a) -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompJoin a (t: ts) accum = case t of

  MonoTSingle d u s st
    | i == di   -> monotoneDecompJoinDown a d u s st ts accum
    | i == ui   -> monotoneDecompJoinUp a d u s st ts accum
    | otherwise -> let (nts, naccum) = monotoneDecompJoin a ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
      (ui, V2 ux uy) = u

  MonoTFork d u ds dst us ust
    | i == di   -> monotoneDecompJoinDown a d u us (N.toList ust) ts (XMonotone a dst ds : accum)
    | i == ui   -> monotoneDecompJoinUp a d u ds (N.toList dst) ts (XMonotone a ust us : accum)
    | otherwise -> let (nts, naccum) = monotoneDecompJoin a ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
      (ui, V2 ux uy) = u
  where
    (i, V2 x y) = a

monotoneDecompJoin _ [] _ = error "Matching track for Join, not found!"

monotoneDecompJoinDown :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompJoinDown a ud uu us ust (t: ts) accum = case t of
  MonoTSingle d u s st -> if i == ui
    then
      ( MonoTFork d uu s (Up u :| st) us (Down ud :| ust) : ts
      , accum
      )
    else
      let (nts, naccum) = monotoneDecompJoinDown a ud uu us ust ts accum in (t: nts, naccum)
    where
      (ui, V2 ux uy) = u
  MonoTFork d u fds fdst fus fust -> if i == ui
    then
      ( MonoTFork d uu fds (Up u <| fdst) us (Down ud :| ust) : ts
      , XMonotone u fust fus : accum )
    else
      let (nts, naccum) = monotoneDecompJoinDown a ud uu us ust ts accum in (t: nts, naccum)
    where
      (ui, V2 ux uy) = u
  where
    (i, V2 x y) = a

monotoneDecompJoinDown _ _ _ _ _ [] _ = error "Matching down track for Join, not found!"

monotoneDecompJoinUp :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompJoinUp a dd du ds dst (t: ts) accum = case t of
  MonoTSingle d u s st -> if i == di
    then
      ( MonoTFork dd u ds (Up du :| dst) s (Down d :| st) : ts
      , accum
      )
    else
      let (nts, naccum) = monotoneDecompJoinUp a dd du ds dst ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
  MonoTFork d u fds fdst fus fust -> if i == di
    then
      ( MonoTFork dd u ds (Up du :| dst) fus (Down d <| fust) : ts
      , XMonotone u fust fus : accum )
    else
      let (nts, naccum) = monotoneDecompJoinUp a dd du ds dst ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
  where
    (i, V2 x y) = a

monotoneDecompJoinUp _ _ _ _ _ [] _ = error "Matching up track for Join, not found!"