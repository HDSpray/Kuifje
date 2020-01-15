module Test where


import Control.Applicative
import qualified Data.Set as ESET

-- tmpf :: (a, Double) -> (a, Double) -> [((a, a), Double)]

tmpf (a, b) (c, d) = [(ESET.fromList [a, c], b*d)]


tmpf2 :: (ESET.Set Integer, Double) -> (Integer, Double) -> (ESET.Set Integer, Double)
tmpf2 (a, b) (c, d) = (ESET.insert c a, b*d)

a = [(1, 1/2), (2, 1/2)]
b = [(4, 1/2), (3, 1/2)]
c = [(1, 1.0)]


t0 = [(ESET.empty, 1)]

tmpfun :: [(Integer, Double)] -> [(ESET.Set Integer, Double)] 
       -> [(ESET.Set Integer, Double)]
tmpfun x y = liftA2 tmpf2 y x

runt = foldr (tmpfun) (t0) [a, b, c]

t = concat $ liftA2 tmpf a b

t2 = liftA2 tmpf2 t c
