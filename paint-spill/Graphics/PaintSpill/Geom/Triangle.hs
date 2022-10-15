{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Graphics.PaintSpill.Geom.Triangle where

import Control.DeepSeq

import Linear

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