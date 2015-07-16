module Database.Backend.PINQuery

import Database.PowerOfPi
import Database.PINQ

data PINQuery : Backend -> Schema -> Stability -> Type  where
  MkPINQuery : Query b s -> PINQuery b s c

where' : PINQuery b s c -> Expr s Bool -> PINQuery b s c
where' (MkPINQuery q) e = MkPINQuery (Select e q)

select : PINQuery b s c -> (f:String -> Maybe  String) -> PINQuery b (projectedSchema f s) c
select (MkPINQuery q) f = MkPINQuery (Projection f q)

union : PINQuery b s c -> PINQuery b s c' -> PINQuery b s (c + c')
union (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Union q q')

intersect : PINQuery b s c -> PINQuery b s c' -> PINQuery b s (c + c')
intersect (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Diff q q')

groupBy : Eq k => Expr s k -> PINQuery b s c -> PINQuery b ["k":::k, "v"::: TableType b s] (c * 2)
groupBy e (MkPINQuery q) = MkPINQuery (GroupBy e q)

lookup : Eq k => k -> PINQuery b ["k":::k, "v"::: TableType b s] c -> PINQuery b s c
lookup k (MkPINQuery q) = MkPINQuery (Lookup k q)

