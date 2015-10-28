module kMeans

import Database.DPDT.Idris
import System.Random.CrapGen
import Data.Floats
import Data.Fin
import Data.Vect
import Data.List
%default total

Point : Schema
Point = [ "x" ::: Float , "y" ::: Float ]

||| Point indexed by a class
data ClassifiedPoint : (k:Nat) -> Type where
  MkClassifiedPoint : (pt:(Float, Float)) -> (cl:Fin k)  -> ClassifiedPoint k


||| Table of points
points : Query Table Point 1
points = MkQuery $ Table [ [ 0  , 0   ]
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

||| Distance between two points using the Pythagorean theorem
dist : (Float,Float) -> (Float,Float) -> Float
dist (x, y) (x', y') = sqrt $ Prelude.Classes.(+) (pow (x - x') 2)  (pow (y - y') 2)

distTest : dist (1,0) (2,0) = 1
distTest = Refl

||| Classifies a point given a vector of classified points, minimizing the dist function
|||
||| @cc Cluster centers
||| @p Point to classify
classify : (cc:Vect (S k) (ClassifiedPoint (S k))) -> (p:(Float, Float)) -> Fin (S k)
classify ((MkClassifiedPoint center cl) :: centers) pt = classify' pt (dist center pt) cl centers where
  classify' : (Float, Float) -> Float -> Fin (S k) -> Vect _ (ClassifiedPoint (S k)) -> Fin (S k)
  classify' pt curDist curClass [] = curClass
  classify' pt curDist curClass ((MkClassifiedPoint center cl) :: centers) = let thisDist = (dist center pt)
                                                                     in if curDist > thisDist
                                                                        then classify' pt thisDist cl centers
                                                                        else classify' pt curDist curClass centers

||| Initial cluster centers
initialCenters : Vect 3 (ClassifiedPoint 3)
initialCenters = [ MkClassifiedPoint (0,  1) 0,
            MkClassifiedPoint (-1,-1) 1,
            MkClassifiedPoint (1,  0) 2 ]

classifyTest : classify initialCenters (1,0.9) = 2
classifyTest = Refl

||| The classify function lifted up to an expression
|||
||| @cc Cluster centers
classifyExpr : (cc : Vect 3 (ClassifiedPoint 3)) -> Expr Point (Fin 3)
classifyExpr centers = PureFn (classify centers) $ Couple (Point^"x") (Point^"y")

||| Update centers by calculating the mean of the points assigned to them
|||
||| @cc Current cluster centers
||| @g Points grouped by cluster class index
||| @e Precission parameter
updateCenters : (cc:Vect n (ClassifiedPoint 3)) -> (g:Grouping Table Point (Fin 3) c) -> (e:Epsilon) -> Vect n $ ClassifiedPoint 3
updateCenters []                                    _      _ = []
updateCenters ((MkClassifiedPoint _ cl) :: centers) groups e =
  let group     = lookup cl groups
      newCenter = evalPrivate ( do x <- noisyAverage (Point^"x") group e
                                   y <- noisyAverage (Point^"y") group e
                                   return (x, y)) 123
  in (MkClassifiedPoint newCenter cl) :: updateCenters centers groups e

||| k-means clustering with x iterations.
kMeans : (x:Nat) -> Vect 3 (ClassifiedPoint 3) -> Query Table Point c -> Epsilon -> Vect 3 (ClassifiedPoint 3)
kMeans Z     centers q e = centers
kMeans (S k) centers q e = let groups = groupBy (classifyExpr centers) q
                           in  kMeans k (updateCenters centers groups e) q e


kMeansTest : Vect 3 (ClassifiedPoint 3)
kMeansTest = kMeans 1 initialCenters points 1

