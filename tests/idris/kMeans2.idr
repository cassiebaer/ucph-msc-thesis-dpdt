module Main

import Data.Fin
import Data.Floats
import Data.Vect
import Database.DPDT.Idris

record Point2 where
  constructor MkPt
  x : Double
  y : Double

dist : Point2 -> Point2 -> Double
dist (MkPt x y) (MkPt x' y') = sqrt $ (x-x')*(x-x') + (y-y')*(y-y')

instance Eq Point2 where
    (==) p p' = dist p p' < 0.000000001

instance Cast Integer Double where
    cast = fromInteger

SPoint2 : Schema
SPoint2 = [ "p" ::: Point2 ]

pGetter : (Point2 -> a) -> Expr SPoint2 a
pGetter f = PureFn f (SPoint2^"p")

point2 : Cast a Double => a -> a -> Row SPoint2
point2 x y = [ MkPt (cast x) (cast y) ]

-- TODO 1 : create smart constructor for DPDT protected tables
points : Query SPoint2 1
points = MkQuery $ Table $ [ point2 x y | x <- [0,1], y <- [0,1] ]
                        ++ [ point2 x y | x <- [7,8], y <- [7,8] ]

labelVect : Vect n a -> Vect n (Nat,a)
labelVect v = go v Z
  where go : Vect n a -> Nat -> Vect n (Nat,a)
        go [] k = []
        go (x :: xs) k = (k,x) :: go xs (S k)

classify : Vect (S n) Point2 -> Point2 -> Fin (S n)
classify {n} cs p = let ds = labelVect (map (dist p) cs)
                     in fromNat . fst $ foldr1 findMinDist ds
  where findMinDist : (Nat,Double) -> (Nat,Double) -> (Nat,Double)
        findMinDist (ix,v) (ix',v') = if (v < v') then (ix,v) else (ix',v')

-- updateCenters : Vect (S n) Point2 -> Grouping SPoint2 (Fin (S n)) c -> Private (c*(2//10)) (Vect (S n) Point2)
-- updateCenters cs g with (labelVect cs)
--   updateCenters cs g | ((ix, c) :: lcs) = do
--     let ps = lookup (fromNat ix) g
--     x <- noisyAverage (pGetter x) ps (1//10)
--     y <- noisyAverage (pGetter y) ps (1//10)
--     return (MkPt x y)

kMeans : (k:Nat) -> (initPts:Vect (S n) Point2) -> (tbl:Query SPoint2 c) -> (eps:Epsilon) -> Private s (Vect (S n) Point2)
--kMeans Z     centers tbl eps = return centers
--kMeans (S k) centers tbl eps = let classifyExpr = PureFn (classify centers) (SPoint2^"p")
                                --in ?kMeans_rhs

