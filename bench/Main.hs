module Main where

import Gauge
import Bench.Geom

main :: IO ()
main = defaultMain [benchGeom]