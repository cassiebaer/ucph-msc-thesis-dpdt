module Database.PowerOfPi.Query

import public Database.PowerOfPi.Types
import public Database.PowerOfPi.Expr
import public Database.Backend.Backend
%default total

||| Represents a typed Query tree.
|||
||| @b The backend dictates the table representation,
|||    and the mappings are defined in TableType
||| @s The schema of attributes for the given query
data Query : (b:Backend) -> (s:Schema) -> Type where
  ||| Represents raw data as a Query.
  Table   : TableType b s -> Query b s
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
  
  GroupBy : Expr s k    -> Query b s -> Query b ["k":::k, "v":::Query b s]

namespace Aggregations

  ||| Represents an aggregation of a typed query tree.
  |||
  ||| @b The backend of the underlying query
  ||| @s The schema of attributes for the underlying query
  data QueryAggregation : (b:Backend) -> (s:Schema) -> a -> Type where
    ||| Represents an arbitrary aggregation
    Aggregation : Query b s -> (a -> a -> a) -> a -> Expr s a -> QueryAggregation b s a
    ||| Represents an arbitrary aggregation over a monoid
    AggregationM : (Monoid a) => Query b s -> Expr s a -> QueryAggregation b s a

