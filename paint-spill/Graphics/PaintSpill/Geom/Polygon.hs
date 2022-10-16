module Graphics.PaintSpill.Geom.Polygon where

import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as N

import Control.DeepSeq

import Linear.V2 (V2 (V2))

import Graphics.PaintSpill.Util (DownUp(Down, Up))
import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Geom.Monotone


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

instance (NFData a) => NFData (MonoMark a) where
    rnf (MonoLeft a b) = rnf a `seq` rnf b
    rnf (MonoRight a b) = rnf a `seq` rnf b
    rnf (MonoStart a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (MonoFork a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (MonoJoin a) = rnf a
    rnf (MonoEnd a) = rnf a

data MonoTrack a
    = MonoTSingle a a a [DownUp a]
    | MonoTFork a a a (NonEmpty (DownUp a)) a (NonEmpty (DownUp a))

makeSortedMarks :: (Ord a, Fractional a) => [(i, V2 a)] -> [MonoMark (i, V2 a)]
makeSortedMarks poly = marks
  where
    cmpVerts (_, V2 ax ay) (_, V2 bx by) = if xcmp == EQ then compare ay by else xcmp
      where
        xcmp = compare ax bx
    
    makeMonoMark (LT, LT, a, b, c) = (b, MonoLeft b c)
    makeMonoMark (GT, GT, a, b, c) = (b, MonoRight b a)
    makeMonoMark (LT, GT, a, b, c)
      | ay < cy   = (b, MonoEnd b)
      | otherwise = (b, MonoJoin b)
      where
        (_, V2 _ ay) = a
        (_, V2 _ cy) = c

    makeMonoMark (GT, LT, a, b, c)
      | ay < cy   = (b, MonoFork a b c)
      | otherwise = (b, MonoStart a b c)
      where
        (_, V2 _ ay) = a
        (_, V2 _ cy) = c

    makeMonoMark _ = error "Not happening!"
  
    pa = tail . cycle $ poly
    pb = tail pa

    segs = zipWith3 (\a b c -> (cmpVerts a b, cmpVerts b c, a, b, c)) poly pa pb
    marksNotSorted =  makeMonoMark <$> segs
    marks = snd <$> sortBy (cmpVerts `on` fst) marksNotSorted


monotoneDecomp :: (Eq i, Ord a, Fractional a) => [(i, V2 a)] -> [XMonotone (i, V2 a)]
monotoneDecomp poly = monotoneDecompGo (makeSortedMarks poly) [] []


monotoneDecompGo :: (Eq i, Ord a) => [MonoMark (i, V2 a)] -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> [XMonotone (i, V2 a)]
monotoneDecompGo [] [] accum = accum
monotoneDecompGo [] _ accum = error "Not concluded tracks."
monotoneDecompGo (m : ms) tracks accum = 
  let (ntracks, naccum) = case m of
        MonoStart a b c -> (MonoTSingle c a b []: tracks, accum)
        MonoLeft b c -> monotoneDecompLeft b c tracks accum
        MonoRight b a -> monotoneDecompRight b a tracks accum
        MonoEnd b -> monotoneDecompEnd b tracks accum
        MonoFork a b c -> monotoneDecompFork a b c tracks accum
        MonoJoin a -> monotoneDecompJoin a tracks accum
  in monotoneDecompGo ms ntracks naccum

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

  MonoTSingle d u s st -> if (dy <= by) && (by <= uy)
    then case st of
      [] ->
        ( MonoTSingle d a s [Up b]
        : MonoTSingle c u s [Down b]
        : ts
        , accum
        )
      (Down hd: nst) ->
        ( MonoTSingle d a hd [Up b]
        : MonoTSingle c u s (Down b : st)
        : ts
        , accum
        )
      (Up hu: nst) ->
        ( MonoTSingle d a s (Up b : st)
        : MonoTSingle c u hu [Down b]
        : ts
        , accum
        )
    else
      let (nts, naccum) = monotoneDecompFork a (bi, V2 bx by)c ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
      (ui, V2 ux uy) = u
  
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
    | i == di   -> monotoneDecompJoinDown a u s st ts accum
    | i == ui   -> monotoneDecompJoinUp a d s st ts accum
    | otherwise -> let (nts, naccum) = monotoneDecompJoin a ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
      (ui, V2 ux uy) = u

  MonoTFork d u ds dst us ust
    | i == di   -> monotoneDecompJoinDown a u us (N.toList ust) ts (XMonotone a dst ds : accum)
    | i == ui   -> monotoneDecompJoinUp a d ds (N.toList dst) ts (XMonotone a ust us : accum)
    | otherwise -> let (nts, naccum) = monotoneDecompJoin a ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
      (ui, V2 ux uy) = u
  where
    (i, V2 x y) = a

monotoneDecompJoin _ [] _ = error "Matching track for Join, not found!"

monotoneDecompJoinDown :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompJoinDown a uu us ust (t: ts) accum = case t of
  MonoTSingle d u s st -> if i == ui
    then
      ( MonoTFork d uu s (Up u :| st) us (Down a :| ust) : ts
      , accum
      )
    else
      let (nts, naccum) = monotoneDecompJoinDown a uu us ust ts accum in (t: nts, naccum)
    where
      (ui, V2 ux uy) = u
  MonoTFork d u fds fdst fus fust -> if i == ui
    then
      ( MonoTFork d uu fds (Up u <| fdst) us (Down a :| ust) : ts
      , XMonotone u fust fus : accum )
    else
      let (nts, naccum) = monotoneDecompJoinDown a uu us ust ts accum in (t: nts, naccum)
    where
      (ui, V2 ux uy) = u
  where
    (i, V2 x y) = a

monotoneDecompJoinDown _ _ _ _ [] _ = error "Matching down track for Join, not found!"

monotoneDecompJoinUp :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> [MonoTrack (i, V2 a)] -> [XMonotone (i, V2 a)] -> ([MonoTrack (i, V2 a)], [XMonotone (i, V2 a)])
monotoneDecompJoinUp a dd ds dst (t: ts) accum = case t of
  MonoTSingle d u s st -> if i == di
    then
      ( MonoTFork dd u ds (Up a :| dst) s (Down d :| st) : ts
      , accum
      )
    else
      let (nts, naccum) = monotoneDecompJoinUp a dd ds dst ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
  MonoTFork d u fds fdst fus fust -> if i == di
    then
      ( MonoTFork dd u ds (Up a :| dst) fus (Down d <| fust) : ts
      , XMonotone u fust fus : accum )
    else
      let (nts, naccum) = monotoneDecompJoinUp a dd ds dst ts accum in (t: nts, naccum)
    where
      (di, V2 dx dy) = d
  where
    (i, V2 x y) = a

monotoneDecompJoinUp _ _ _ _ [] _ = error "Matching up track for Join, not found!"