module Graphics.PaintSpill.Geom.Triangle where

import Control.DeepSeq

import Linear

-- | A Triangle type.
-- 
-- Triangle is just a bunch of 3 data.
--
data Triangle i a = Triangle (i, V2 a) (i, V2 a) (i, V2 a) deriving (Eq)


instance (NFData i, NFData a) => NFData (Triangle i a) where
    rnf (Triangle (ai, av) (bi, bv) (ci, cv)) = rnf ai `seq` rnf av `seq` rnf bi `seq` rnf bv `seq` rnf ci `seq` rnf cv

-- | Unindexed triangle.
unindexedTriangle :: V2 a -> V2 a -> V2 a -> Triangle () a
unindexedTriangle a b c = Triangle ((), a) ((), b) ((), c)

-- | Membership check for triangle, as shape.
triElem :: (Ord a, Fractional a) => Triangle i a -> V2 a -> Bool
triElem (Triangle (_, a) (_, b) (_, c)) e = (0 <= i) && (0 <= j) && (i + j <= 1)
  where
    ab = b - a
    ac = c - a
    ae = e - a
    m = V2 ab ac
    V2 i j = ae *! inv22 m


-- | Indices as tuple.
triIndices :: Triangle i a -> (i, i, i)
triIndices (Triangle (ai, _) (bi, _) (ci, _)) = (ai, bi, ci)