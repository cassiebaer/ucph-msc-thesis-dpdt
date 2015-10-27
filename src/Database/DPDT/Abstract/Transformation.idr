module Database.DPDT.Abstract.PINQuery

import Data.Rational
import Database.PowerOfPi
import Database.DPDT.Abstract.Types

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

