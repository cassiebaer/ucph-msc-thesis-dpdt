module Database.PowerOfPi.Idris

import Data.Dictionary
import Data.List
import public Database.PowerOfPi

%default total

||| Represents a row in a table.
||| We impose an Eq constraint for the Expr language.
data Row : Schema -> Type where
  Nil  : Row []
  (::) : Eq t => t -> Row s -> Row (name:::t::s)

(++) : Row s -> Row s' -> (Row (s ++ s'))
(++) []      ys = ys
(++) (x::xs) ys = x :: xs ++ ys

instance Eq (Row s) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

project : (f:String -> Maybe String) -> Row s -> Row (projectedSchema f s)
project f []      {s=[]}        = []
project f (r::rs) {s=n:::t::as} with (f n)
  project f (r::rs) {s=n:::t::as} | Nothing = project f rs
  project f (r::rs) {s=n:::t::as} | Just n' = r :: project f rs

----------------------------------------------------------------

||| Get the value of an attribute given a proof that the
||| attribute exists
lookupVal : (Row s) -> (nm:String) -> (p : (map cast s) `ContainsKey` nm) -> lookupType s p
lookupVal (x::xs) nm Here       = x
lookupVal (x::xs) nm (There s') = lookupVal xs nm s'

||| Evaluates an Expr in the context of a row.
eval : Expr s t -> Row s -> t
eval (Lit x)        _ = x
eval (x + y)        r = eval x r + eval y r
eval (x - y)        r = eval x r - eval y r
eval (x / y)        r = eval x r / eval y r
eval (x * y)        r = eval x r * eval y r
eval ((^) _ nm {p}) r = lookupVal r nm p
eval (x == y)       r = eval x r == eval y r
eval (x /= y)       r = not $ eval x r == eval y r
eval (Couple x y)   r = (eval x r, eval y r)
eval (PureFn f x)   r = f (eval x r)

----------------------------------------------------------------

Table : Schema -> Type
Table s = List (Row s)

GroupingMap : Type -> Schema -> Type
GroupingMap k s = Dictionary k (List $ Row s)

mkGroupingMap : Eq k => Expr s k -> Table s -> GroupingMap k s
mkGroupingMap e []      = []
mkGroupingMap e (r::rs) = insertWith (++) (eval e r) [r] (mkGroupingMap e rs)

mkPartitionMap : Eq k => List k -> Expr s k -> Table s -> GroupingMap k s
mkPartitionMap ks e []      = []
mkPartitionMap ks e (r::rs) with (eval e r `elem` ks)
  mkPartitionMap ks e (r::rs) | False = mkPartitionMap ks e rs
  mkPartitionMap ks e (r::rs) | True = insertWith (++) (eval e r) [r] (mkPartitionMap ks e rs)

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
    eval (Lookup k group) = lookupWithDefault [] k (eval group)

  namespace Grouping

    %assert_total
    eval : (q:Grouping Table s k) -> GroupingMap k s
    eval (MkGrouping e q) = mkGroupingMap e (eval q)

  namespace Partitioning

    eval : (q:Partitioning Table s k) -> GroupingMap k s
    eval (MkPartitioning ks e q) = mkPartitionMap ks e (eval q)

  namespace Aggregation

    eval : Aggregation Table s a -> a
    eval (MkAggregation  q f z e) = foldr f z (map (eval e) (eval q))
    eval (MkAggregationM q e)     = foldr (<+>) neutral (map (eval e) (eval q))
