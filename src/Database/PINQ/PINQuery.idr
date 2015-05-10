module PINQuery

import Data.Rational
import Database.PowerOfPi
import Database.PINQ.Types

data PINQuery : Schema -> Stability -> Type where
    MkLeaf   : Query s -> PINQuery s c
    MkUnary  : Query s -> PINQuery s c' -> PINQuery s c 
    MkBinary : Query s -> PINQuery s c' -> PINQuery s c'' -> PINQuery s c

getQuery : PINQuery s _ -> Query s
getQuery (MkLeaf q) = q
getQuery (MkUnary q _) = q
getQuery (MkBinary q _ _) = q

table : List (Row s) -> PINQuery s 1
table rs = MkLeaf (Table rs)

where' : PINQuery s c -> Expr s Bool -> PINQuery s c 
where' pq p = MkUnary (Select p (getQuery pq)) pq
       
--select : PINQuery s c -> (f:String -> Maybe String)-> PINQuery (projectedSchema f s) c
--select (MkPINQuery q) f = MkUnary (Projection f q)

union : PINQuery s c -> PINQuery s c' -> PINQuery s (c * c')
union pq1 pq2 = MkBinary (Union (getQuery pq1) (getQuery pq2)) pq1 pq2

--intersection : PINQuery s c -> PINQuery s c' -> PINQuery s (c * c')
--intersection (MkPINQuery q) (MkPINQuery q') = MkPINQuery (Diff q q')

