module Database.PowerOfPi.Abstract.Query

import Database.PowerOfPi.Abstract.Types
import public Database.PowerOfPi.Abstract.Expr
%default total

mutual
  ||| Represents a typed Query tree.
  |||
  ||| @t The actual representation of a table.
  ||| @s The schema of attributes for the given query
  data Query : (t:Schema -> Type) -> (s:Schema) -> Type where
    ||| Represents raw data as a Query.
    Table   : t s -> Query t s
    ||| Represents the union of two queries.
    Union   : Query t s -> Query t s -> Query t s
    ||| Represents the set difference of two queries.
    Diff    : Query t s -> Query t s -> Query t s
    ||| Represents the cartesian product of two queries.
    ||| N.B. Currently not safe because Disjoint is not implemented.
    Product : Query t s -> Query t s' -> { auto p : Disjoint s s' } -> Query t (s ++ s')
    ||| Represents the projection of a new schema onto a Query.
    Projection : (f:String -> Maybe String) -> Query t s -> Query t (projectedSchema f s)
    ||| Represents selection on a Query using the given expression.
    Select  : Expr s Bool -> Query t s -> Query t s
    ||| Represents a lookup into the result of a GroupBy
    Lookup  : Eq k => k -> Grouping t s k -> Query t s

  data Grouping : (t:Schema -> Type) -> (s:Schema) -> (k:Type) -> Type where
    ||| Represents the grouping of a query into an associative map
    GroupBy : Eq k => Expr s k -> Query t s -> Grouping t s k

  data Partitioning : (t:Schema -> Type) -> (s:Schema) -> (k:Type) -> Type where
    ||| Represents the partitioning of a query into a dictionary
    Partition : Eq k => List k -> Expr s k -> Query t s -> Partitioning t s k

getSchema : Query b s -> Schema
getSchema {s} _ = s

namespace Aggregations

  ||| Represents an aggregation of a typed query tree.
  |||
  ||| @a The resulting type of the aggregation
  data QueryAggregation : (t:Schema -> Type) -> (s:Schema) -> (a:Type) -> Type where
    ||| Represents an arbitrary aggregation
    Aggregation : Query t s -> (a -> a -> a) -> a -> Expr s a -> QueryAggregation t s a
    ||| Represents an arbitrary aggregation over a monoid
    AggregationM : (Monoid a) => Query t s -> Expr s a -> QueryAggregation t s a

namespace CommonAggregations

  ||| Counts the number of rows in a Query
  count : Query t s -> QueryAggregation t s Nat
  count q = Aggregation q (+) 0 (Lit 1)

  ||| Computes the sum of the expression applied to each row.
  sum : Num a => Query t s -> Expr s a -> QueryAggregation t s a
  sum q e = Aggregation q (+) 0 e

