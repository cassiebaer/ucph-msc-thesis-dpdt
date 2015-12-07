module Main

import Data.Vect
import Database.DPDT.Idris
import System.Random.CrapGen

------------------------------------------------------------------------------

record Point2 where
  constructor MkPt
  ptX : Double
  ptY : Double

dist : Point2 -> Point2 -> Double
dist (MkPt x y) (MkPt x' y') = sqrt $ (x-x')*(x-x') + (y-y')*(y-y')

instance Eq Point2 where
    (==) x y = dist x y < (1/1000000)

------------------------------------------------------------------------------

SPoint2 : Schema
SPoint2 = [ "x" ::: Double, "y" ::: Double ]

------------------------------------------------------------------------------

points : Query SPoint2 1
points = MkQuery $ Table $ [ point2 x y | x <- [0,1], y <- [0,1] ]
                        ++ [ point2 x y | x <- [7,8], y <- [7,8] ]
  where point2 : Cast a Double => a -> a -> Row SPoint2
        point2 x y = [ (cast x), (cast y) ]

------------------------------------------------------------------------------

labelVect : Vect n a -> Vect n (Nat,a)
labelVect v = go v Z
  where go : Vect n a -> Nat -> Vect n (Nat,a)
        go [] k        = []
        go (x :: xs) k = (k,x) :: go xs (S k)

-- classifies each point and gives an ix to the center it matched
classify : Vect (S n) Point2 -> Point2 -> Fin (S n)
classify {n} cs p = let ds = map (dist p) cs
                     in fromNat . fst $ foldr1 findMinDist (labelVect ds)
  where findMinDist : (Nat,Double) -> (Nat,Double) -> (Nat,Double)
        findMinDist (ix,d) (ix',d') = if (d < d') then (ix,d) else (ix',d')

eps : Epsilon
eps = (1//2)

updateCenters : Vect (S n) Point2 -> Grouping SPoint2 (Fin (S n)) c
             -> Private ?updateCenters_cost (Vect (S n) Point2)
updateCenters cs gr = sequence (map (updateCenter gr) (map (fromNat . fst) $ labelVect cs))
  where
    updateCenter : Grouping SPoint2 (Fin (S n)) c -> Fin (S n) -> Private (c*eps + c*eps) Point2
    updateCenter gr ix = do
      let ps = lookup ix gr
      x <- noisyAverage (SPoint2^"x") ps eps
      y <- noisyAverage (SPoint2^"y") ps eps
      return (MkPt x y)

kMeans : (k:Nat) -> (cs:Vect (S n) Point2) -> (tbl:Query SPoint2 1) -> Private ?kMeans_cost (Vect (S n) Point2)
kMeans Z     cs tbl = return cs
kMeans (S k) cs tbl = do
    cs' <- updateCenters cs (groupBy classifyExpr tbl)
    kMeans k cs' tbl
  where classifyExpr = PureFn (classify cs) (PureFn (uncurry MkPt) (Couple (SPoint2^"x") (SPoint2^"y")))

foo : Vect 2 Point2
foo = evalPrivate (kMeans 10 [MkPt 3 4, MkPt 5 6] points) 123

---------- Proofs ----------

Main.updateCenters_cost = proof
  intros
  search

testDist : dist (MkPt 0 0) (MkPt 0 1) = 1
testDist = Refl

