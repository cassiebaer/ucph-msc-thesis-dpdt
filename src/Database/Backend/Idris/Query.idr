module Database.Backend.Idris.Query

import Database.PowerOfPi.Query
import Database.Backend.Idris.Row
import Database.Backend.Idris.Expr
import Database.Backend.Backend
%default total

namespace Query
 
  put : Eq k => k -> Row s -> List $ Row ["k":::k, "v":::List (Row s)] -> List $ Row ["k":::k, "v":::List (Row s)]
  put k r []               = [[k, [r]]]
  put k r ([k', rs] :: ks) = if k == k' then [ k', (r::rs) ] :: ks
                                        else [ k' , rs ] :: put k r ks

  lookup : Eq k => k -> List $ Row ["k":::k, "v":::List (Row s)] -> List (Row s)
  lookup k [] = []
  lookup k ([k', rs] :: ks) = if k == k' then rs
                                         else lookup k ks

  --Essentially fold TODO: Use fold
  constructMap : Eq k => Expr s k -> List (Row s) -> List $ Row ["k":::k, "v":::List (Row s)]
  constructMap e []      = []
  constructMap e (r::rs) = put (eval e r) r $ constructMap e rs
  
  ||| Evaluates a Query, returning a List of Rows.
  eval : (q:Query Idris s) -> List (Row s)
  eval (Table xs)       = xs
  eval (Union x y)      = (eval x) ++ (eval y)
  eval (Diff x y)       = (eval x) \\ (eval y)
  eval (Product x y)    = [ x' ++ y' | x' <- eval x, y' <- eval y ]
  eval (Projection f x) = map (project f) (eval x)
  eval (Select e x)     = filter (eval e) (eval x)
  eval (GroupBy e x)    = constructMap e $ eval x
  eval (Lookup k group) = lookup k (eval group)

namespace QueryAggregation
  eval : QueryAggregation Idris s a -> a
  eval (Aggregation  q f z e) = foldr f z (map (eval e) (eval q))
  eval (AggregationM q e) = foldr (<+>) neutral (map (eval e) (eval q))
