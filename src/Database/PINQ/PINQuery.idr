module PINQuery

import Data.Rational
import Database.PowerOfPi

Sensitivity : Type
Sensitivity = Rational

Stability : Type
Stability = Rational

data PINQuery : Schema -> Stability -> Type where
    MkPINQuery : Query s -> PINQuery s c 

data PINQueryAggregation : Schema -> a -> Sensitivity -> Type where
    MkPINQueryAggregation : QueryAggregation s a -> PINQueryAggregation s a c

table : List (Row s) -> PINQuery s (1:%1)
table rs = MkPINQuery (Table rs)

where' : PINQuery s _ -> (Row s -> Bool) -> PINQuery s (1:%1) 
where' (MkPINQuery q) p = MkPINQuery (Select (RowFn p) q)

select : PINQuery s _ -> (f:String -> Maybe String)-> PINQuery (projectedSchema f s) (1:%1)
select (MkPINQuery q) f = MkPINQuery (Projection f q)

union : PINQuery s _ -> PINQuery s _ -> PINQuery s (1:%1)
union (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Union q q')

intersection : PINQuery s _ -> PINQuery s _ -> PINQuery s (1:%1)
intersection (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Diff q q')

