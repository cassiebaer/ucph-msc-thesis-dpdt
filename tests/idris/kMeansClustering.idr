import Database.PowerOfPi
import Database.PINQ
import Database.Backend.PINQuery
import Database.Backend.Idris.PINQuery
import Database.Backend.Idris.Row
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
points = MkPINQuery $ Table [ [ 0 , 0 ]
                            , [ 1 , 0 ]
                            , [ 0 , 1 ]
                            , [ 1 , 1 ]
                            , [ 3 , 2 ]
                            , [ 3 , 3 ]
                            ]

dist : (Float,Float) -> (Float,Float) -> Float
dist (x, y) (x', y') = sqrt $ Prelude.Classes.(+) (pow (x - x') 2)  (pow (y - y') 2)

distTest : dist (1,0) (2,0) = 1
distTest = Refl

classify : (l : List (ClassifiedPoint k)) -> {auto ok : NonEmpty l} -> (Float, Float) -> Fin k
classify []                                  {ok=IsNonEmpty} _ impossible
classify ((MkClassifiedPoint center cl) :: centers) pt = classify' pt (dist center pt) cl centers where
  classify' : (Float, Float) -> Float -> Fin k -> List (ClassifiedPoint k) -> Fin k
  classify' pt curDist curClass [] = curClass
  classify' pt curDist curClass ((MkClassifiedPoint center cl) :: centers) = let thisDist = (dist center pt)
                                                                     in if curDist > thisDist
                                                                        then classify' pt thisDist cl centers 
                                                                        else classify' pt curDist curClass centers
centers : List (ClassifiedPoint 3)
centers = [ MkClassifiedPoint (0,0) 0,
            MkClassifiedPoint (5,5) 1,
            MkClassifiedPoint (-1,0) 2 ]

classifyTest : classify centers (1,1) = 0 
classifyTest = Refl

classifyExpr : Expr Point (Fin 3) 
classifyExpr = PureFn (classify centers) $ Couple (Point^"x") (Point^"y")

updateCenters : (l:List (ClassifiedPoint 3)) -> {auto ok : NonEmpty l} -> PINQuery Idris ["k":::(Fin 3), "v":::(List (Row Point)) ] c -> Epsilon -> List $ ClassifiedPoint 3
updateCenters [] {ok=IsNonEmpty} _ _ impossible
updateCenters ((MkClassifiedPoint _ cl) :: centers) groups e = 
  let group = lookup cl groups
      newCenter = evalPrivate ( do x <- noisyAverage (Point^"x") group e
                                   y <- noisyAverage (Point^"y") group e
                                   return (x, y)) 123
  in (MkClassifiedPoint newCenter cl) :: updateCenters centers groups e

kMeans : Nat -> (l : List (ClassifiedPoint 3)) -> {auto ok : NonEmpty l} -> PINQuery Idris Point c -> Epsilon -> List (ClassifiedPoint 3)
kMeans _     [] {ok=IsNonEmpty} _ _ impossible 
kMeans Z     centers q e = centers
kMeans (S k) centers q e = let groups = groupBy classifyExpr q
                           in  kMeans k (updateCenters centers groups e) q e
