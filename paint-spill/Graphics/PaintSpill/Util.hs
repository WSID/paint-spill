module Graphics.PaintSpill.Util where

import Control.DeepSeq
import Data.Foldable

data DownUp a = Down a | Up a deriving (Eq, Ord, Show, Read)

instance Functor DownUp where
    fmap func (Down a) = Down (func a)
    fmap func (Up a) = Up (func a)

instance NFData a => NFData (DownUp a) where
    rnf (Down a) = rnf a
    rnf (Up a) = rnf a


splitList :: (Foldable f) => f (DownUp a) -> ([a], [a])
splitList = foldr splitAccumList ([], [])

splitAccumList :: DownUp a -> ([a], [a]) -> ([a], [a])
splitAccumList (Down a) (a1, a2) = (a : a1, a2)
splitAccumList (Up a) (a1, a2) = (a1, a : a2)