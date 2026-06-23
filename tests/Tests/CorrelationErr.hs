module Tests.CorrelationErr where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Numeric.Sum (Summation (add, zero), kbn)
import Statistics.Sample

xy :: [(Double, Double)]
xy = [(-10.0, -9.66), (3.0, 4.0), (-8.55, 9.0), (6.03, -3.0), (3.0, 3.0), (0.4, 0.9), (-2.0, -6.0), (0.342, 7.4), (0.92, -9.16), (3.666, -7.0)]

xyv :: V.Vector (Double, Double)
xyv = V.fromList xy

muX = expectation fst xyv

muY = expectation snd xyv

xs = map fst xy

ys = map snd xy

n = fromIntegral $ length xy

muXAct = sum xs / n

muYAct = sum ys / n

testExpect f xs =
  kbn (G.foldl' (\s -> add s . f) zero xs)

testExpectWOKbn f xs =
  G.foldl' (\s -> add s . f) zero xs

sumX = testExpect fst xyv

sumXAct = sum xs
