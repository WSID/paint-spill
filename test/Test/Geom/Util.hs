module Test.Geom.Util where

import Control.Applicative
import Control.Monad

import Data.Bool
import Data.Functor.Compose

import Linear

import Test.Tasty.HUnit


-- Makes grid membership distribution
makeCoordList :: V2 Float -> V2 Float -> V2 Float -> [[V2 Float]]
makeCoordList (V2 sx sy) (V2 nx ny) (V2 ex ey) =
    (\y -> (`V2` y) <$> [sx, nx .. ex]) <$> reverse [sy, ny .. ey]

coordList :: [[V2 Float]]
coordList = makeCoordList (V2 (-4) (-4)) (V2 (-3.75) (-3.75)) (V2 4 4)

ccoordList :: Compose ZipList ZipList (V2 Float)
ccoordList = Compose (ZipList (ZipList <$> coordList))

(@#?=) :: Compose ZipList ZipList Bool -> Compose ZipList ZipList Bool -> Assertion
actual @#?= expected = do
    when (getCompose actual /= getCompose expected) $ do
        let res False False = ' '
            res True True = '*'
            res False True = 'e'
            res True False = 'a' 
            Compose difference = liftA2 res actual expected
        
        assertFailure $ "Two lists are different:\n\n" ++ (unlines . getZipList . fmap getZipList  $ difference)

(@#=?) :: Compose ZipList ZipList Bool -> Compose ZipList ZipList Bool -> Assertion
expected @#=? actual = actual @#?= expected