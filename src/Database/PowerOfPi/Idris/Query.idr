module Database.PowerOfPi.Idris.Query

import Data.Dictionary
import Database.PowerOfPi.Abstract
import Database.PowerOfPi.Idris.Expr
import Database.PowerOfPi.Idris.Types
%default total

Table : Schema -> Type
Table s = List (Row s)

GroupingMap : Type -> Schema -> Type
GroupingMap k s = Dictionary k (List $ Row s)

mkGroupingMap : Eq k => Expr s k -> Table s -> GroupingMap k s
mkGroupingMap e []      = []
mkGroupingMap e (r::rs) = insertWith (++) (eval e r) [r] (mkGroupingMap e rs)

mutual

  namespace Query

    ||| Evaluates a Query, returning a Table s.
    %assert_total
    eval : (q:Query Table s) -> Table s
    eval (Table xs)       = xs
    eval (Union x y)      = (eval x) ++ (eval y)
    eval (Diff x y)       = (eval x) \\ (eval y)
    eval (Product x y)    = [ x' ++ y' | x' <- eval x, y' <- eval y ]
    eval (Projection f x) = map (project f) (eval x)
    eval (Select e x)     = filter (eval e) (eval x)
    --eval (GroupBy e x)    = constructMap e $ eval x
    eval (Lookup k group) = lookupWithDefault [] k (eval group)

  namespace Grouping

    %assert_total
    eval : (q:Grouping Table s k) -> GroupingMap k s
    eval (GroupBy e q) = mkGroupingMap e (eval q)

namespace QueryAggregation

  eval : QueryAggregation Table s a -> a
  eval (Aggregation  q f z e) = foldr f z (map (eval e) (eval q))
  eval (AggregationM q e)     = foldr (<+>) neutral (map (eval e) (eval q))

