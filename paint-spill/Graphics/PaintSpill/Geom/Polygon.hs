module Graphics.PaintSpill.Geom.Polygon where

import Control.Applicative
import Data.Maybe
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as N

import Control.DeepSeq

import Linear.V2 (V2 (V2))

import Graphics.PaintSpill.Util (DownUp(Down, Up))
import Graphics.PaintSpill.Geom
import Graphics.PaintSpill.Geom.Monotone

cmpPtrs :: (Ord a) => V2 a -> V2 a -> Ordering
cmpPtrs (V2 ax ay) (V2 bx by) = case compare ax bx of
  EQ -> compare ay by
  cmp -> cmp

cmpVerts :: (Ord a) => (i, V2 a) -> (i, V2 a) -> Ordering
cmpVerts = compare `on` snd


-- Denotes non-zero filling rule.
nonZero :: Int -> Bool
nonZero = (/= 0)

-- Denotes even-odd filling rule.
evenOdd :: Int -> Bool
evenOdd = odd

polyElem :: (Ord a, Fractional a) => (Int -> Bool) -> V2 a -> [[V2 a]] -> Bool
polyElem fill point poly = or (fill <$> [fmin .. fmax])
  where
    segify strips = zip4 strips (drop 1 sc) (drop 2 sc) (drop 3 sc)
      where
        sc = cycle strips
    
    segs = foldMap segify poly

    step v sls (fmin, fmax)
      | bx < cx = stepRight
      | cx < bx = stepLeft
      | by < cy = stepUp
      | cy < by = stepDown
      where
        V2 x y = v
        (_, b, c, _) = sls
        (V2 ax ay, V2 bx by, V2 cx cy, V2 dx dy) = sls

        stepRight
          | ((bx < x) && (x < cx)) || ((cx == x) && (cx < dx))
          = case compare y (segY b c x) of
              LT -> (pred fmin, pred fmax)
              EQ -> (pred fmin, fmax)
              GT -> (fmin, fmax)
          | (cx == x) && (dx < cx)  = if y == cy then (pred fmin, fmax) else (fmin, fmax)
          | otherwise               = (fmin, fmax)

        stepLeft
          | ((cx < x) && (x < bx)) || ((cx == x) && (dx < cx))
          = case compare y (segY b c x) of
              LT -> (succ fmin, succ fmax)
              EQ -> (fmin, succ fmax)
              GT -> (fmin, fmax)
          | (cx == x) && (cx < dx)  = if y == cy then (fmin, succ fmax) else (fmin, fmax)
          | otherwise               = (fmin, fmax)
        
        stepUp
          | (x == bx) && (by <= y) && (y <= cy) = case compare dx cx of
              LT -> (fmin, succ fmax)
              EQ -> (fmin, fmax)
              GT -> (pred fmin, fmax)
          | otherwise = (fmin, fmax)
        
        stepDown
          | (x == bx) && (cy <= y) && (y <= by) = case compare ax bx of
              LT -> (pred fmin, fmax)
              EQ -> (fmin, fmax)
              GT -> (fmin, succ fmax)
          | otherwise = (fmin, fmax)

    (fmin, fmax) = foldr (step point) (0, 0) segs


polyElemEvenOdd :: (Ord a, Fractional a) => V2 a -> [[V2 a]] -> Bool
polyElemEvenOdd = polyElem evenOdd

polyElemNonZero :: (Ord a, Fractional a) => V2 a -> [[V2 a]] -> Bool
polyElemNonZero = polyElem nonZero

data MonoMark i a
    = MonoLeft (i, V2 a) (i, V2 a)
    | MonoRight (i, V2 a) (i, V2 a)
    | MonoSNeg (i, V2 a) (i, V2 a) (i, V2 a)
    | MonoSPos (i, V2 a) (i, V2 a) (i, V2 a) 
    | MonoENeg (i, V2 a)
    | MonoEPos (i, V2 a)
    deriving (Show)

instance (NFData i, NFData a) => NFData (MonoMark i a) where
    rnf (MonoLeft (ai, av) (bi, bv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv
    rnf (MonoRight (ai, av) (bi, bv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv
    rnf (MonoSNeg (ai, av) (bi, bv) (ci, cv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv `seq` rnf ci `seq` rnf cv
    rnf (MonoSPos (ai, av) (bi, bv) (ci, cv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv `seq` rnf ci `seq` rnf cv
    rnf (MonoENeg (ai, av)) = rnf ai `seq` rnf av
    rnf (MonoEPos (ai, av)) = rnf ai `seq` rnf av

data TrackDir = TrackLeft | TrackRight deriving (Eq, Show)

-- TODO: Lint up track data.
data MonoTrack i a = MonoTrack
    { monoTrackOverlaps :: Int
    , monoTrackOutline :: Bool
    , monoTrackDown :: MonoSegTrack i a
    , monoTrackUp :: MonoSegTrack i a
    , monoTrackStrip :: MonoTrackStrips i a
    }
    deriving Show

data MonoSegTrack i a = MonoSegTrack
    { monoSegTrackDir :: TrackDir
    , monoSegTrackPrev :: (i, V2 a)
    , monoSegTrackCurr :: (i, V2 a)
    }
    deriving Show

data MonoTrackStrips i a
    = MonoStripSingle
    { monoSingleStart :: (i, V2 a)
    , monoSingleStrip :: [DownUp (i, V2 a)]
    }
    | MonoStripFork
    { monoForkDownStart :: (i, V2 a)
    , monoForkDownStrip :: NonEmpty (DownUp (i, V2 a))
    , monoForkUpStart :: (i, V2 a)
    , monoForkUpStrip :: NonEmpty (DownUp (i, V2 a))
    }
    deriving Show 

makeMonoMarks :: (Ord a) => [(i, V2 a)] -> [((i, V2 a), MonoMark i a)]
makeMonoMarks winding = marks
  where
    makeMonoMark (LT, LT, a, b, c) = (b, MonoLeft b c)
    makeMonoMark (GT, GT, a, b, c) = (b, MonoRight b a)
    makeMonoMark (LT, GT, a, b, c)
      | ay < cy   = (b, MonoEPos b)
      | otherwise = (b, MonoENeg b)
      where
        (_, V2 _ ay) = a
        (_, V2 _ cy) = c

    makeMonoMark (GT, LT, a, b, c)
      | ay < cy   = (b, MonoSNeg a b c)
      | otherwise = (b, MonoSPos a b c)
      where
        (_, V2 _ ay) = a
        (_, V2 _ cy) = c
    
    pa = tail . cycle $ winding
    pb = tail pa

    segs = zipWith3 (\a b c -> (cmpVerts a b, cmpVerts b c, a, b, c)) winding pa pb
    marks = makeMonoMark <$> segs


makeSortedMarks :: (Ord a, Fractional a) => [[(i, V2 a)]] -> [MonoMark i a]
makeSortedMarks poly = marks
  where
    marksNotSorted =  foldMap makeMonoMarks poly
    marks = snd <$> sortBy (cmpVerts `on` fst) marksNotSorted


matchSegTrackLeft :: (Eq i) => (i, V2 a) -> (i, V2 a) -> MonoSegTrack i a -> Maybe ((i, V2 a), MonoSegTrack i a)
matchSegTrackLeft _ _ (MonoSegTrack TrackRight _ _) = Nothing
matchSegTrackLeft a b (MonoSegTrack TrackLeft prev curr)
  | i == ai   = Just (curr, MonoSegTrack TrackLeft curr b)
  | otherwise = Nothing
  where
    i = fst curr
    ai = fst a

matchSegTrackRight :: (Eq i) => (i, V2 a) -> (i, V2 a) -> MonoSegTrack i a -> Maybe ((i, V2 a), MonoSegTrack i a)
matchSegTrackRight _ _ (MonoSegTrack TrackLeft _ _) = Nothing
matchSegTrackRight a b (MonoSegTrack TrackRight prev curr)
  | i == ai   = Just (curr, MonoSegTrack TrackRight curr b)
  | otherwise = Nothing
  where
    i = fst curr
    ai = fst a

isSegTrackVertical :: (Ord a) => MonoSegTrack i a -> Bool
isSegTrackVertical (MonoSegTrack _ (_, V2 px _) (_, V2 cx _)) = px == cx

segYSegTrack :: (Ord a, Fractional a) => MonoSegTrack i a -> a -> a
segYSegTrack (MonoSegTrack _ (_, pv) (_, cv)) x = segY pv cv x 

data MonotoneDecomp i a = MonotoneDecomp (Int -> Bool) [MonoTrack i a] [XMonotone i a]

pushTrack :: MonoTrack i a -> MonotoneDecomp i a -> MonotoneDecomp i a
pushTrack t (MonotoneDecomp fill ts ms) = MonotoneDecomp fill (t: ts) ms

pushMonotone :: XMonotone i a -> MonotoneDecomp i a -> MonotoneDecomp i a
pushMonotone m (MonotoneDecomp fill ts ms) = MonotoneDecomp fill ts (m: ms)

popTrack :: MonotoneDecomp i a -> Maybe (MonotoneDecomp i a, MonoTrack i a)
popTrack (MonotoneDecomp fill (t: ts) ms) = Just (MonotoneDecomp fill ts ms, t)
popTrack _ = Nothing 


monotoneDecomp :: (Eq i, Ord a, Fractional a, Show i, Show a) => (Int -> Bool) -> [[(i, V2 a)]] -> [XMonotone i a]
monotoneDecomp fill poly = monotoneDecompGo fill (makeSortedMarks poly) (MonotoneDecomp fill [] [])


monotoneDecompNonZero :: (Eq i, Ord a, Fractional a, Show i, Show a) => [[(i, V2 a)]] -> [XMonotone i a]
monotoneDecompNonZero = monotoneDecomp nonZero

monotoneDecompEvenOdd :: (Eq i, Ord a, Fractional a, Show i, Show a) => [[(i, V2 a)]] -> [XMonotone i a]
monotoneDecompEvenOdd = monotoneDecomp evenOdd

monotoneDecompGo :: (Eq i, Ord a, Fractional a, Show i, Show a) => (Int -> Bool) -> [MonoMark i a] -> MonotoneDecomp i a -> [XMonotone i a]
monotoneDecompGo fill ms md = let MonotoneDecomp fill tracks accum = foldl (flip go) md ms in case tracks of
  [] -> accum
  _ -> error "Not concluded tracks."
  where
    go (MonoSPos a b c) = pushTrack MonoTrack
        { monoTrackOverlaps = 1
        , monoTrackOutline = (fill 0 /= fill 1)
        , monoTrackDown = MonoSegTrack TrackLeft b c
        , monoTrackUp = MonoSegTrack TrackRight b a
        , monoTrackStrip = MonoStripSingle b []
        }
    go (MonoLeft b c) = monotoneDecompLeft b c
    go (MonoRight b a) = monotoneDecompRight b a
    go (MonoEPos b) = monotoneDecompEPos b
    go (MonoSNeg a b c) = monotoneDecompSNeg a b c
    go (MonoENeg a) = monotoneDecompENeg a

monotoneDecompLeft :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompLeft (ai, av) b md = case popTrack md of
  Just (nmd, t)
    | dd == TrackLeft && di == ai -> case ts of
        MonoStripSingle s st        -> nmd
          & pushTrack t {monoTrackDown = MonoSegTrack TrackLeft d b, monoTrackStrip = MonoStripSingle s (Down d : st)}
        MonoStripFork ds dst us ust -> nmd
          & pushTrack t {monoTrackDown = MonoSegTrack TrackLeft d b, monoTrackStrip = MonoStripSingle us (Down d : N.toList ust)}
          & if inc then pushMonotone (XMonotone d dst ds) else id

    | ud == TrackLeft && ui == ai -> case ts of
        MonoStripSingle s st        -> nmd
          & pushTrack t {monoTrackUp = MonoSegTrack TrackLeft u b, monoTrackStrip = MonoStripSingle s (Up u : st)}
        MonoStripFork ds dst us ust -> nmd
          & pushTrack t {monoTrackUp = MonoSegTrack TrackLeft u b, monoTrackStrip = MonoStripSingle ds (Up u : N.toList dst)}
          & if inc then pushMonotone (XMonotone u ust us) else id

    | otherwise -> pushTrack t $ monotoneDecompLeft (ai, av) b nmd
    where
      MonoTrack _ inc (MonoSegTrack dd _ d) (MonoSegTrack ud _ u) ts = t
      (di, dv) = d
      (ui, uv) = u

  Nothing -> error "Matching for left mark, not found!"


monotoneDecompRight :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompRight (ai, av) b md = case popTrack md of
  Just (nmd, t)
    | dd == TrackRight && di == ai -> case ts of
        MonoStripSingle s st        -> nmd
          & pushTrack t {monoTrackDown = MonoSegTrack TrackRight d b, monoTrackStrip = MonoStripSingle s (Down d : st)}
        MonoStripFork ds dst us ust -> nmd
          & pushTrack t {monoTrackDown = MonoSegTrack TrackRight d b, monoTrackStrip = MonoStripSingle us (Down d : N.toList ust)}
          & if inc then pushMonotone (XMonotone d dst ds) else id

    | ud == TrackRight && ui == ai -> case ts of
        MonoStripSingle s st        -> nmd
          & pushTrack t {monoTrackUp = MonoSegTrack TrackRight u b, monoTrackStrip = MonoStripSingle s (Up u : st)}
        MonoStripFork ds dst us ust -> nmd
          & pushTrack t {monoTrackUp = MonoSegTrack TrackRight u b, monoTrackStrip = MonoStripSingle ds (Up u : N.toList dst)}
          & if inc then pushMonotone (XMonotone u ust us) else id

    | otherwise -> pushTrack t $ monotoneDecompRight (ai, av) b nmd
    where
      MonoTrack _ inc (MonoSegTrack dd _ d) (MonoSegTrack ud _ u) ts = t
      (di, dv) = d
      (ui, uv) = u

  Nothing -> error "Matching for right mark, not found!"


monotoneDecompEPos :: (Eq i, Ord a) => (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompEPos b md = case popTrack md of
  Just (nmd, t)
    | dd == TrackLeft && ud == TrackRight && di == bi && ui == bi -> case ts of
        MonoStripSingle s (sth : stn) -> if inc then pushMonotone (XMonotone b (sth :| stn) s) nmd else nmd
        MonoStripFork ds dst us ust -> if inc then nmd
          & pushMonotone (XMonotone b (Up u <| ust) us)
          & pushMonotone (XMonotone b (Down d <| dst) ds)
        else nmd
    | otherwise -> pushTrack t $ monotoneDecompEPos b nmd
    where
      MonoTrack _ inc (MonoSegTrack dd _ d) (MonoSegTrack ud _ u) ts = t
      (bi, bv) = b
      (di, dv) = d
      (ui, uv) = u

  Nothing -> error "Matching track for End not found!"


monotoneDecompSNeg :: (Ord a, Fractional a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompSNeg a b c md = case popTrack md of
  Just (nmd, t)
    | (dy <= by) && (by <= uy) -> case ts of
        MonoStripSingle s st -> case st of
          [] -> nmd
            & pushTrack t {monoTrackDown = MonoSegTrack TrackLeft b c, monoTrackStrip = MonoStripSingle s [Down b]}
            & pushTrack t {monoTrackUp = MonoSegTrack TrackRight b a, monoTrackStrip = MonoStripSingle s [Up b]}
            -- pushTrack (MonoTrack ne nout ...)
          (Down hd: nst) -> nmd
            & pushTrack t {monoTrackDown = MonoSegTrack TrackLeft b c, monoTrackStrip = MonoStripSingle s (Down b: st)}
            & pushTrack t {monoTrackUp = MonoSegTrack TrackRight b a, monoTrackStrip = MonoStripSingle hd [Up b]}
          (Up hu: nst) -> nmd
            & pushTrack t {monoTrackDown = MonoSegTrack TrackLeft b c, monoTrackStrip = MonoStripSingle hu [Down b]}
            & pushTrack t {monoTrackUp = MonoSegTrack TrackRight b a, monoTrackStrip = MonoStripSingle s (Up b: st)}
        MonoStripFork ds dst us ust -> nmd
          & pushTrack t {monoTrackDown = MonoSegTrack TrackLeft b c, monoTrackUp = u, monoTrackStrip = MonoStripSingle us (Down b: N.toList ust)}
          & pushTrack t {monoTrackDown = d, monoTrackUp = MonoSegTrack TrackRight b a, monoTrackStrip = MonoStripSingle ds (Up b: N.toList dst)}
    | otherwise -> pushTrack t $ monotoneDecompSNeg a b c nmd
    where
      MonoTrack _ inc d u ts = t
      (bi, V2 bx by) = b
      MonoSegTrack dd _ dc = d
      MonoSegTrack ud _ uc = u
      dy = if isSegTrackVertical d then let (_, V2 _ cy) = dc in cy else segYSegTrack d bx
      uy = if isSegTrackVertical u then let (_, V2 _ cy) = uc in cy else segYSegTrack u bx

  Nothing -> pushTrack
      MonoTrack
        { monoTrackOverlaps = (-1)
        , monoTrackOutline = not (fill 0) && fill (-1)
        , monoTrackDown = MonoSegTrack TrackRight b a
        , monoTrackUp = MonoSegTrack TrackLeft b c
        , monoTrackStrip = MonoStripSingle b []
        }
      md
  where
    MonotoneDecomp fill _ _ = md

monotoneDecompENeg :: (Eq i, Ord a) => (i, V2 a) -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompENeg a md = case popTrack md of
  Just (nmd, t)
    | i == di && i == ui && dd == TrackRight && ud == TrackLeft -> case ts of
        MonoStripSingle s (sth : stn) -> if inc then pushMonotone (XMonotone a (sth :| stn) s) nmd else nmd
        MonoStripFork ds dst us ust -> if inc then nmd
          & pushMonotone (XMonotone a (Up u <| ust) us)
          & pushMonotone (XMonotone a (Down d <| dst) ds)
        else nmd

    | i == di -> case ts of
        MonoStripSingle s st -> monotoneDecompENegDown a u s st nmd
        MonoStripFork ds dst us ust -> monotoneDecompENegDown a u us (N.toList ust) (pushMonotone (XMonotone a dst ds) nmd)

    | i == ui -> case ts of
        MonoStripSingle s st -> monotoneDecompENegUp a d s st nmd
        MonoStripFork ds dst us ust -> monotoneDecompENegUp a d ds (N.toList dst) (pushMonotone (XMonotone a ust us) nmd)
    | otherwise -> pushTrack t $ monotoneDecompENeg a nmd
    where
      MonotoneDecomp fill _ _ = md
      MonoTrack _ inc (MonoSegTrack dd _ d) (MonoSegTrack ud _ u) ts = t
      (i, V2 x y) = a
      (di, V2 dx dy) = d
      (ui, V2 ux uy) = u

  Nothing -> error "Matching track for Join, not found!"

monotoneDecompENegDown :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompENegDown a pu us ust md = case popTrack md of
  Just (nmd, t)
    | i == ui -> case ts of
        MonoStripSingle s st -> pushTrack t {monoTrackUp = MonoSegTrack ud u pu, monoTrackStrip = MonoStripFork s (Up u :| st) us (Down a :| ust)} nmd
        MonoStripFork fds fdst fus fust -> nmd
          & pushTrack t {monoTrackUp = MonoSegTrack ud u pu, monoTrackStrip = MonoStripFork fds (Up u <| fdst) us (Down a :| ust)}
          & pushMonotone (XMonotone u fust fus)
    | otherwise -> pushTrack t $ monotoneDecompENegDown a pu us ust nmd
    where
      MonoTrack _ inc (MonoSegTrack dd _ d) (MonoSegTrack ud _ u) ts = t
      (ui, V2 ux uy) = u
      (i, V2 x y) = a

  Nothing -> error "Matching down track for Join, not found!"

monotoneDecompENegUp :: (Eq i, Ord a) => (i, V2 a) -> (i, V2 a) -> (i, V2 a) -> [DownUp (i, V2 a)] -> MonotoneDecomp i a -> MonotoneDecomp i a
monotoneDecompENegUp a pd ds dst md = case popTrack md of
  Just (nmd, t)
    | i == di -> case ts of
        MonoStripSingle s st -> pushTrack t {monoTrackDown = MonoSegTrack dd d pd, monoTrackStrip = MonoStripFork ds (Up a :| dst) s (Down d :| st)} nmd
        MonoStripFork fds fdst fus fust -> nmd
          & pushTrack t {monoTrackDown = MonoSegTrack dd d pd, monoTrackStrip = MonoStripFork ds (Up a :| dst) fus (Down d <| fust)}
          & pushMonotone (XMonotone u fust fus)
    | otherwise -> pushTrack t $ monotoneDecompENegUp a pd ds dst nmd
    where
      MonoTrack _ inc (MonoSegTrack dd _ d) (MonoSegTrack ud _ u) ts = t
      (di, V2 dx dy) = d
      (i, V2 x y) = a

  Nothing -> error "Matching up track for Join, not found!"