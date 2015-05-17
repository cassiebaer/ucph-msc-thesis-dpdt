module Database.PowerOfPi.Query
import Database.PowerOfPi.Types
import Database.PowerOfPi.Expr
%default total

data Backend = Idris | SQL

TableType : Backend -> Schema-> Type
TableType Idris s = List (Row s)
TableType SQL   _ = String

||| Represents a typed Query tree.
|||
||| @s The schema of attributes for the given query
data Query : (b:Backend) ->(s:Schema) -> Type where
  ||| Represents raw data as a Query.
  Table   : (TableType b s) -> Query b s
  ||| Represents the union of two queries.
  Union   : Query b s -> Query b s -> Query b s
  ||| Represents the set difference of two queries.
  Diff    : Query b s -> Query b s -> Query b s
  ||| Represents the cartesian product of two queries.
  ||| N.B. Currently not safe because Disjoint is not implemented.
  Product : Query b s -> Query b s' -> { auto p : Disjoint s s' } -> Query b (s ++ s')
  ||| Represents the projection of a new schema onto a Query.
  Projection : (f:String -> Maybe String) -> Query b s -> Query b (projectedSchema f s)
  ||| Represents selection on a Query using the given expression.
  Select  : Expr s Bool -> Query b s -> Query b s

||| Evaluates a Query, returning a List of Rows.
eval : Query Idris s -> List (Row s)
eval (Table xs) = xs
eval (Union x y) = eval x ++ eval y
eval (Diff x y) = (eval x) \\ (eval y)
eval (Product x y) = [ x' ++ y' | x' <- eval x, y' <- eval y ]
eval (Projection f x) = map (project f) (eval x)
eval (Select e x) = filter (evalExpr e) (eval x)

namespace Aggregations

  ||| Represents an aggregation of a typed query tree.
  |||
  ||| @s The schema of attributes for the underlying query
  data QueryAggregation : (b:Backend) -> (s:Schema) -> a -> Type where
    ||| Represents an arbitrary aggregation
    Aggregation : Query b s -> (a -> a -> a) -> a -> Expr s a -> QueryAggregation b s a
    ||| Represents an arbitrary aggregation over a monoid
    AggregationM : (Monoid a) => Query b s -> Expr s a -> QueryAggregation b s a

  eval : QueryAggregation Idris s a -> a
  eval (Aggregation  q f z x) = foldr f z (map (evalExpr x) (eval q))
  eval (AggregationM q x) = foldr (<+>) neutral (map (evalExpr x) (eval q))

