module Database.Backend.Idris.Query

import Database.PowerOfPi.Query
import Database.Backend.Idris.Row
import Database.Backend.Idris.Expr

namespace Query
  ||| Evaluates a Query, returning a List of Rows.
  eval : Query Idris s -> List (Row s)
  eval (Table xs) = xs
  eval (Union x y) = eval x ++ eval y
  eval (Diff x y) = (eval x) \\ (eval y)
  eval (Product x y) = [ x' ++ y' | x' <- eval x, y' <- eval y ]
  eval (Projection f x) = map (project f) (eval x)
  eval (Select e x) = filter (eval e) (eval x)

namespace QueryAggregation
  eval : QueryAggregation Idris s a -> a
  eval (Aggregation  q f z e) = foldr f z (map (eval e) (eval q))
  eval (AggregationM q e) = foldr (<+>) neutral (map (eval e) (eval q))
