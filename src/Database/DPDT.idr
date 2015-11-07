module Database.DPDT

import Data.Vect
import Data.Rational
import Database.PowerOfPi
import Statistics.Distribution.Laplace
import System.Random.CrapGen
%default total

----------------------------------------------------------------

Epsilon : Type
Epsilon = Rational

Sensitivity : Type
Sensitivity = Rational

Stability : Type
Stability = Rational

data Query : (Schema -> Type) -> Schema -> Stability -> Type  where
  MkQuery : Query b s -> Query b s c

data Grouping : (Schema -> Type) -> Schema -> Type -> Stability -> Type where
  MkGrouping : Grouping t s k -> Grouping t s k c

----------------------------------------------------------------

-- Transformations work by unwrapping the Query from a Query and rewrapping it with a new data and type constructor

where' : Query b s c -> Expr s Bool -> Query b s c
where' (MkQuery q) e = MkQuery (Select e q)

select : Query b s c -> (f:String -> Maybe  String) -> Query b (projectedSchema f s) c
select (MkQuery q) f = MkQuery (Projection f q)

union : Query b s c -> Query b s c' -> Query b s (c + c')
union (MkQuery q) (MkQuery q') = MkQuery (Union q q')

intersect : Query b s c -> Query b s c' -> Query b s (c + c')
intersect (MkQuery q) (MkQuery q') = MkQuery (Diff q q')

groupBy : (Eq k, Show k) => Expr s k -> Query b s c -> Grouping b s k (c * 2)
groupBy e (MkQuery q) = MkGrouping (MkGrouping e q)

lookup : (Eq k, Show k) => k -> Grouping b s k c -> Query b s c
lookup k (MkGrouping q) = MkQuery (Lookup k q)

----------------------------------------------------------------

||| Represents a Private computation
data Private : Sensitivity -> Type -> Type where
  MkPrivate : (CrapGen -> (a,CrapGen)) -> Private budget a

||| Evaluates a Private computation
evalPrivate : Private s a -> CrapGen -> a
evalPrivate (MkPrivate f) g = fst (f g)

||| Lifts a value into a Private computation.
return : a -> Private s a
return x = MkPrivate $ \s => (x,s)

||| Sequencing primitive. Allows us to overload Idris' do-notation
(>>=) : Private s a -> (a -> Private s' b) -> Private (s + s') b
(>>=) (MkPrivate sf) f = MkPrivate $ \st => let (x,st1)       = sf st
                                                MkPrivate sf' = f x
                                            in sf' st1
