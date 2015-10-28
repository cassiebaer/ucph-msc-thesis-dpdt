module Database.PowerOfPi
%default total

namespace Attribute

  ||| Represents a table attribute.
  data Attribute : Type where
    (:::) : String -> Type -> Attribute
  infix 8 :::

  ||| Attribute equality is based only on their names!
  instance Eq Attribute where
    (==) (n:::t) (n':::t') = n == n'

  instance Cast Attribute (Pair String Type) where
    cast (n ::: t) = (n,t)

namespace Schema

  ||| Represents a table.
  Schema : Type
  Schema = List Attribute

  getNames : Schema -> List String
  getNames s = map (fst . cast) s

  projectedSchema : (f:String -> Maybe String) -> Schema -> Schema
  projectedSchema f [] = []
  projectedSchema f (n:::t::as) = maybe (projectedSchema f as) (\n' => n':::t::projectedSchema f as) (f n)

namespace Proofs

  -- TODO : Implement disjoint!
  ||| A proof that two schemas are disjoint.
  ||| N.B. This is currently not implemented.
  data Disjoint : Schema -> Schema -> Type where
    FakeDisjoint : Disjoint xs ys

  -- TODO : Contribute ContainsKey to Idris HEAD
  -- https://github.com/idris-lang/Idris-dev/pull/2202

  ||| A proof that an association list contains the given key.
  data ContainsKey : List (k,v) -> k -> Type where
    Here  : ContainsKey ((k,v)::ps) k
    There : ContainsKey ps k -> ContainsKey ((k',v)::ps) k

  instance Uninhabited (ContainsKey [] k) where
      uninhabited Here      impossible
      uninhabited (There p) impossible

  decContainsKey : DecEq 'k => (ps:List ('k,'v)) -> (k:'k) -> Dec (ps `ContainsKey` k)
  decContainsKey [] k = No absurd
  decContainsKey ((a, b) :: ps) k with (decEq a k)
    decContainsKey ((k, b) :: ps) k | (Yes Refl) = Yes Here
    decContainsKey ((a, b) :: ps) k | (No notHere) with (ps `decContainsKey` k)
      decContainsKey ((a, b) :: ps) k | (No notHere) | (Yes prf) = Yes (There prf)
      decContainsKey ((a, b) :: ps) k | (No notHere) | (No notThere) = No (mkNo notHere notThere)
        where
          mkNo : ((k' = k) -> Void) ->
                 (ps `ContainsKey` k -> Void) ->
                 ((k',v')::ps) `ContainsKey` k -> Void
          mkNo f g Here = f Refl
          mkNo f g (There x) = g x

----------------------------------------------------------------

||| Looks up the Type associated with 'nm'
|||
||| @ps The schema to check
||| @k  The name to look for
%assert_total
lookupType : (ps:Schema) -> (map cast ps) `ContainsKey` k -> Type
lookupType (k:::v :: ps) Here = v
lookupType (k':::v :: ps) (There x) = lookupType ps x

||| Represents a typed expression.
|||
||| @s The attributes available to the expression
||| @t The return type of the expression
data Expr : (s:Schema) -> (t:Type) -> Type where
  ||| Fetches the value of the given attribute for the current row.
  |||
  ||| @s  The schema of available attributes (i.e. the current row)
  ||| @nm The name of the attribute to look up
  (^) : (s:Schema) -> (nm:String) -> { auto p : (map cast s) `ContainsKey` nm } -> Expr s (lookupType s p)
  (+) : Num t  => Expr s t -> Expr s t -> Expr s t
  (-) : Num t  => Expr s t -> Expr s t -> Expr s t
  (/) : Expr s Float -> Expr s Float -> Expr s Float
  (*) : Num t  => Expr s t -> Expr s t -> Expr s t
  (==): Eq t   => Expr s t -> Expr s t -> Expr s Bool
  (/=): Eq t   => Expr s t -> Expr s t -> Expr s Bool
  Lit : Show t => (val:t) -> Expr s t
  Couple : Expr s t -> Expr s t' -> Expr s $ Pair t t'
  PureFn : (a -> b) -> Expr s a -> Expr s b

infixl 9 ^

----------------------------------------------------------------

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
    ||| Represents a lookup into the result of a Grouping
    Lookup  : Eq k => k -> Grouping t s k -> Query t s

  ||| Represents the grouping of a query into an associative map
  data Grouping : (t:Schema -> Type) -> (s:Schema) -> (k:Type) -> Type where
    MkGrouping : Eq k => Expr s k -> Query t s -> Grouping t s k

  ||| Represents the partitioning of a query into a dictionary
  data Partitioning : (t:Schema -> Type) -> (s:Schema) -> (k:Type) -> Type where
    MkPartitioning : Eq k => List k -> Expr s k -> Query t s -> Partitioning t s k

namespace Aggregations

  ||| Represents an aggregation of a typed query tree.
  |||
  ||| @a The resulting type of the aggregation
  data Aggregation : (t:Schema -> Type) -> (s:Schema) -> (a:Type) -> Type where
    ||| Represents an arbitrary aggregation
    MkAggregation : Query t s -> (a -> a -> a) -> a -> Expr s a -> Aggregation t s a
    ||| Represents an arbitrary aggregation over a monoid
    MkAggregationM : (Monoid a) => Query t s -> Expr s a -> Aggregation t s a

namespace CommonAggregations

  ||| Counts the number of rows in a Query
  count : Query t s -> Aggregation t s Nat
  count q = MkAggregation q (+) 0 (Lit 1)

  ||| Computes the sum of the expression applied to each row.
  sum : Num a => Query t s -> Expr s a -> Aggregation t s a
  sum q e = MkAggregation q (+) 0 e

