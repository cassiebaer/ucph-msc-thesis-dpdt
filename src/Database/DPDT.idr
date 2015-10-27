module Database.DPDT

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

data PINQuery : (Schema -> Type) -> Schema -> Stability -> Type  where
  MkPINQuery : Query b s -> PINQuery b s c

data PINGrouping : (Schema -> Type) -> Schema -> Type -> Stability -> Type where
  MkPINGrouping : Grouping t s k -> PINGrouping t s k c

----------------------------------------------------------------

-- Transformations work by unwrapping the Query from a PINQuery and rewrapping it with a new data and type constructor

where' : PINQuery b s c -> Expr s Bool -> PINQuery b s c
where' (MkPINQuery q) e = MkPINQuery (Select e q)

select : PINQuery b s c -> (f:String -> Maybe  String) -> PINQuery b (projectedSchema f s) c
select (MkPINQuery q) f = MkPINQuery (Projection f q)

union : PINQuery b s c -> PINQuery b s c' -> PINQuery b s (c + c')
union (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Union q q')

intersect : PINQuery b s c -> PINQuery b s c' -> PINQuery b s (c + c')
intersect (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Diff q q')

groupBy : Eq k => Expr s k -> PINQuery b s c -> PINGrouping b s k (c * 2)
groupBy e (MkPINQuery q) = MkPINGrouping (GroupBy e q)

lookup : Eq k => k -> PINGrouping b s k c -> PINQuery b s c
lookup k (MkPINGrouping q) = MkPINQuery (Lookup k q)

----------------------------------------------------------------

||| Represents a Private computation
data Private : Sensitivity -> Type -> Type where
  MkPrivate : (CrapGen -> (a,CrapGen)) -> Private budget a

||| Evaluates a Private computation
evalPrivate : Private s a -> CrapGen -> a
evalPrivate (MkPrivate f) g = fst (f g)

||| Lifts a value into a Private computation.
return : a -> Private 0 a
return x = MkPrivate $ \s => (x,s)

||| Sequencing primitive. Allows us to overload Idris' do-notation
(>>=) : Private s a -> (a -> Private s' b) -> Private (s + s') b
(>>=) (MkPrivate sf) f = MkPrivate $ \st => let (x,st1)       = sf st
                                                MkPrivate sf' = f x
                                            in sf' st1