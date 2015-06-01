import Database.PowerOfPi
import Database.PINQ
import Database.Backend.PINQuery
import Database.Backend.Idris.PINQuery
import Database.Backend.Idris.Row
import System.Random.CrapGen
import Data.Floats
import Data.Fin
import Data.Vect

Point : Schema
Point = [ "x" ::: Float , "y" ::: Float ]

data NonZero : Nat -> Type

data ClassifiedPoint : (k:Nat) -> Type where
  MkClassifiedPoint : (pt:(Float, Float)) -> (cl:Fin k) -> ClassifiedPoint k

getClass : ClassifiedPoint k -> Fin k
getClass (MkClassifiedPoint _ cl) = cl

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

classify : (pt:(Float, Float)) -> Vect (S n) (ClassifiedPoint (S n))  -> ClassifiedPoint (S n)
classify pt ((MkClassifiedPoint center cl) :: centers) = f pt (dist center pt) cl centers where
  f : (Float, Float) -> Float -> Fin n -> Vect m (ClassifiedPoint n) -> ClassifiedPoint n
  f pt curDist curClass [] = MkClassifiedPoint pt curClass
  f pt curDist curClass ((MkClassifiedPoint center cl) :: centers) = let thisDist = (dist center pt)
                                                                     in if curDist > thisDist
                                                                        then f pt thisDist cl centers 
                                                                        else f pt curDist curClass centers
centers : Vect 3 (ClassifiedPoint 3)
centers = [ MkClassifiedPoint (0,0) 0,
            MkClassifiedPoint (5,5) 1,
            MkClassifiedPoint (-1,0) 2 ]

classifyTest : getClass $ classify (1,1) centers = 0 
classifyTest = Refl
