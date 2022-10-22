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


data MonoMark i a
    = MonoLeft (i, V2 a) (i, V2 a)
    | MonoRight (i, V2 a) (i, V2 a)
    | MonoStart (i, V2 a) (i, V2 a) (i, V2 a) 
    | MonoFork (i, V2 a) (i, V2 a) (i, V2 a)
    | MonoJoin (i, V2 a)
    | MonoEnd (i, V2 a)
    deriving (Show)

instance (NFData i, NFData a) => NFData (MonoMark i a) where
    rnf (MonoLeft (ai, av) (bi, bv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv
    rnf (MonoRight (ai, av) (bi, bv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv
    rnf (MonoStart (ai, av) (bi, bv) (ci, cv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv `seq` rnf ci `seq` rnf cv
    rnf (MonoFork (ai, av) (bi, bv) (ci, cv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv `seq` rnf ci `seq` rnf cv
    rnf (MonoJoin (ai, av)) = rnf ai `seq` rnf av
    rnf (MonoEnd (ai, av)) = rnf ai `seq` rnf av

data MonoTrack i a
    = MonoTSingle (i, V2 a) (i, V2 a) (i, V2 a) [DownUp (i, V2 a)]
    | MonoTFork (i, V2 a) (i, V2 a) (i, V2 a) (NonEmpty (DownUp (i, V2 a))) (i, V2 a) (NonEmpty (DownUp (i, V2 a)))

makeSortedMarks :: (Ord a, Fractional a) => [(i, V2 a)] -> [MonoMark i a]
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


data MonotoneDecomp i a = MonotoneDecomp [MonoTrack i a] [XMonotone i a]

pushTrack :: MonoTrack i a -> MonotoneDecomp i a -> MonotoneDecomp i a
pushTrack t (MonotoneDecomp ts ms) = MonotoneDecomp (t: ts) ms

pushMonotone :: XMonotone i a -> MonotoneDecomp i a -> MonotoneDecomp i a
pushMonotone m (MonotoneDecomp ts ms) = MonotoneDecomp ts (m: ms)

popTrack :: MonotoneDecomp i a -> Maybe (MonotoneDecomp i a, MonoTrack i a)
popTrack (MonotoneDecomp (t: ts) ms) = Just (MonotoneDecomp ts ms, t)
popTrack _ = Nothing 

monotoneDecomp :: (Eq i, Ord a, Fractional a) => [(i, V2 a)] -> [XMonotone i a]
monotoneDecomp poly = monotoneDecompGo (makeSortedMarks poly) (MonotoneDecomp [] [])


monotoneDecompGo :: (Eq i, Ord a) => [MonoMark i a] -> MonotoneDecomp i a -> [XMonotone i a]
monotoneDecompGo ms md = let MonotoneDecomp tracks accum = foldl (flip go) md ms in case tracks of
  [] -> accum
  _ -> error "Not concluded tracks."
  where
    go (MonoStart a b c) = pushTrack (MonoTSingle c a b [])
    go (MonoLeft b c) = monotoneDecompLeft b c
    go (MonoRight b a) = monotoneDecompRight b a
    go (MonoEnd b) = monotoneDecompEnd b
    go (MonoFork a b c) = monotoneDecompFork a b c
    go (MonoJoin a) = monotoneDecompJoin a

monotoneDecompLeft :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompLeft (ai, av) b md = case popTrack md of
  Just (nmd, t) -> case t of
    MonoTSingle (di, dv) u s st -> if ai == di
      then pushTrack (MonoTSingle b u s (Down (di, dv) : st)) nmd
      else pushTrack t $ monotoneDecompLeft (ai, av) b nmd

    MonoTFork (di, dv) u ds dst us ust -> if ai == di
      then nmd
        & pushTrack (MonoTSingle b u us (Down (di, dv) : N.toList ust)) 
        & pushMonotone (XMonotone (di, dv) dst ds) 
      else pushTrack t $ monotoneDecompLeft (ai, av) b nmd

  Nothing -> error "Matching not found!"


monotoneDecompRight :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompRight (ai, av) b md = case popTrack md of
  Just (nmd, t) -> case t of
    MonoTSingle d (ui, uv) s st -> if ai == ui
      then pushTrack (MonoTSingle d b s (Up (ui, uv) : st)) nmd
      else pushTrack t $ monotoneDecompRight (ai, av) b nmd

    MonoTFork d (ui, uv) ds dst us ust -> if ai == ui
      then nmd
        & pushTrack ( MonoTSingle d b ds (Up (ui, uv) : N.toList dst) )
        & pushMonotone ( XMonotone (ui, uv) ust us )
      else pushTrack t $ monotoneDecompRight (ai, av) b nmd

  Nothing -> error "Matching not found!"


monotoneDecompEnd :: (Eq i, Ord a) => (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompEnd (bi, bv) md = case popTrack md of
  Just (nmd, t) -> case t of
    MonoTSingle (di, V2 dx dy) (ui, V2 ux uy) s st -> if bi == di
      then if bi == ui
        then case st of
          (sth : stn) -> pushMonotone (XMonotone (bi, bv) (sth :| stn) s) nmd
          [] -> error "A monotone with 2 vertices!"
        else
          error "Partial matching track for end: downside is matching."
      else if bi == ui
        then error "Partial matching track for end: upside is matching."
        else pushTrack t $ monotoneDecompEnd (bi, bv) nmd

    MonoTFork (di, V2 dx dy) (ui, V2 ux uy) ds dst us ust -> if bi == di
      then if bi == ui
        then nmd
          & pushMonotone (XMonotone (bi, bv) (Up (ui, V2 ux uy) <| ust) us)
          & pushMonotone (XMonotone (bi, bv) (Down (di, V2 dx dy) <| dst) ds)
        else
          error "Partial matching track for end: downside is matching."
      else if bi == ui
        then error "Partial matching track for end: upside is matching."
        else pushTrack t $ monotoneDecompEnd (bi, bv) nmd
  Nothing -> error "Matching track for End not found!"


monotoneDecompFork :: (Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompFork a b c md = case popTrack md of
  Just (nmd, t) -> case t of

    MonoTSingle d u s st -> if (dy <= by) && (by <= uy)
      then case st of
        [] -> nmd
          & pushTrack (MonoTSingle c u s [Down b])
          & pushTrack (MonoTSingle d a s [Up b])
        (Down hd: nst) -> nmd
          & pushTrack (MonoTSingle c u s (Down b : st))
          & pushTrack ( MonoTSingle d a hd [Up b] )
        (Up hu: nst) -> nmd
          & pushTrack (MonoTSingle c u hu [Down b])
          & pushTrack (MonoTSingle d a s (Up b : st))
      else
        pushTrack t $ monotoneDecompFork a (bi, V2 bx by) c nmd
      where
        (di, V2 dx dy) = d
        (ui, V2 ux uy) = u
    
    MonoTFork (di, V2 dx dy) (ui, V2 ux uy) ds dst us ust -> if (dy <= by) && (by <= uy)
      then nmd
        & pushTrack (MonoTSingle c (ui, V2 ux uy) us (Down b : N.toList ust))
        & pushTrack (MonoTSingle (di, V2 dx dy) a ds (Up b : N.toList dst))
      else
        pushTrack t $ monotoneDecompFork a (bi, V2 bx by) c nmd
    where
      (bi, V2 bx by) = b

  Nothing ->  error "Suitable track for Fork, not found!"

monotoneDecompJoin :: (Eq i, Ord a) => (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompJoin a md = case popTrack md of
  Just (nmd, t) -> case t of

    MonoTSingle d u s st
      | i == di   -> monotoneDecompJoinDown a u s st nmd
      | i == ui   -> monotoneDecompJoinUp a d s st nmd
      | otherwise -> pushTrack t $ monotoneDecompJoin a nmd
      where
        (di, V2 dx dy) = d
        (ui, V2 ux uy) = u

    MonoTFork d u ds dst us ust
      | i == di   -> monotoneDecompJoinDown a u us (N.toList ust) (pushMonotone (XMonotone a dst ds) nmd)
      | i == ui   -> monotoneDecompJoinUp a d ds (N.toList dst) (pushMonotone (XMonotone a ust us) nmd)
      | otherwise -> pushTrack t $ monotoneDecompJoin a nmd
      where
        (di, V2 dx dy) = d
        (ui, V2 ux uy) = u
    where
      (i, V2 x y) = a

  Nothing -> error "Matching track for Join, not found!"

monotoneDecompJoinDown :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompJoinDown a uu us ust md = case popTrack md of
  Just (nmd, t) -> case t of
    MonoTSingle d u s st -> if i == ui
      then pushTrack (MonoTFork d uu s (Up u :| st) us (Down a :| ust)) nmd
      else pushTrack t $ monotoneDecompJoinDown a uu us ust nmd
      where
        (ui, V2 ux uy) = u
    MonoTFork d u fds fdst fus fust -> if i == ui
      then nmd
        & pushTrack (MonoTFork d uu fds (Up u <| fdst) us (Down a :| ust))
        & pushMonotone (XMonotone u fust fus)
      else pushTrack t $ monotoneDecompJoinDown a uu us ust nmd
      where
        (ui, V2 ux uy) = u
    where
      (i, V2 x y) = a

  Nothing -> error "Matching down track for Join, not found!"

monotoneDecompJoinUp :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompJoinUp a dd ds dst md = case popTrack md of
  Just (nmd, t) -> case t of
    MonoTSingle d u s st -> if i == di
      then pushTrack (MonoTFork dd u ds (Up a :| dst) s (Down d :| st)) nmd
      else pushTrack t $ monotoneDecompJoinUp a dd ds dst nmd
      where
        (di, V2 dx dy) = d
    MonoTFork d u fds fdst fus fust -> if i == di
      then nmd
        & pushTrack (MonoTFork dd u ds (Up a :| dst) fus (Down d <| fust))
        & pushMonotone (XMonotone u fust fus)
      else pushTrack t $ monotoneDecompJoinUp a dd ds dst nmd
      where
        (di, V2 dx dy) = d
    where
      (i, V2 x y) = a

  Nothing -> error "Matching up track for Join, not found!"