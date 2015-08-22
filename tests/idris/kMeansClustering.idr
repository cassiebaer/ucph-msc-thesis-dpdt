module kMeans

import Database.PINQ.Idris
import System.Random.CrapGen
import Data.Floats
import Data.Fin
import Data.Vect
import Data.List
%default total

Point : Schema
Point = [ "x" ::: Float , "y" ::: Float ]

data ClassifiedPoint : (k:Nat) -> Type where
  MkClassifiedPoint : (pt:(Float, Float)) -> (cl:Fin k)  -> ClassifiedPoint k

points : PINQuery Idris Point 1
points = MkPINQuery $ Table [ [ 0  , 0   ]
                            , [ 0.2, 0   ]
                            , [ 0.2 ,0.1 ]
                            , [ 0.5 , 0.5 ]
                            , [ 0.9 , 0.5 ]
                            , [ 0.8 , 0.7 ]
                            , [ -0.8 , -0.7 ]
                            , [ -1   , -0.7 ]
                            , [ -0.8 , -0.5 ]
                            , [ -0.8 , -0.9 ]
                            , [ -0.3 , -0.2 ]
                            ]

dist : (Float,Float) -> (Float,Float) -> Float
dist (x, y) (x', y') = sqrt $ Prelude.Classes.(+) (pow (x - x') 2)  (pow (y - y') 2)

distTest : dist (1,0) (2,0) = 1
distTest = Refl

classify : Vect (S k) (ClassifiedPoint (S k)) -> (Float, Float) -> Fin (S k)
classify ((MkClassifiedPoint center cl) :: centers) pt = classify' pt (dist center pt) cl centers where
  classify' : (Float, Float) -> Float -> Fin (S k) -> Vect _ (ClassifiedPoint (S k)) -> Fin (S k)
  classify' pt curDist curClass [] = curClass
  classify' pt curDist curClass ((MkClassifiedPoint center cl) :: centers) = let thisDist = (dist center pt)
                                                                     in if curDist > thisDist
                                                                        then classify' pt thisDist cl centers 
                                                                        else classify' pt curDist curClass centers
initialCenters : Vect 3 (ClassifiedPoint 3)
initialCenters = [ MkClassifiedPoint (0,  1) 0,
            MkClassifiedPoint (-1,-1) 1,
            MkClassifiedPoint (1,  0) 2 ]

classifyTest : classify initialCenters (1,0.9) = 2 
classifyTest = Refl

classifyExpr : Vect 3 (ClassifiedPoint 3) -> Expr Point (Fin 3) 
classifyExpr centers = PureFn (classify centers) $ Couple (Point^"x") (Point^"y")

updateCenters : Vect n (ClassifiedPoint 3) -> PINQuery Idris ["k":::(Fin 3), "v":::(List (Row Point)) ] c -> Epsilon -> Vect n $ ClassifiedPoint 3
updateCenters []                                    _      _ = []
updateCenters ((MkClassifiedPoint _ cl) :: centers) groups e = 
  let group     = lookup cl groups
      newCenter = evalPrivate ( do x <- noisyAverage (Point^"x") group e
                                   y <- noisyAverage (Point^"y") group e
                                   return (x, y)) 123
  in (MkClassifiedPoint newCenter cl) :: updateCenters centers groups e

kMeans : Nat -> Vect 3 (ClassifiedPoint 3) -> PINQuery Idris Point c -> Epsilon -> Vect 3 (ClassifiedPoint 3)
kMeans Z     centers q e = centers
kMeans (S k) centers q e = let groups = groupBy (classifyExpr centers) q
                           in  kMeans k (updateCenters centers groups e) q e


kMeansTest : Vect 3 (ClassifiedPoint 3)
kMeansTest = kMeans 1 initialCenters points 1

