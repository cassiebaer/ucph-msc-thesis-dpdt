module Database.PowerOfPi.Query
import Data.So
import Database.PowerOfPi.Types
import Database.PowerOfPi.Expr
%default total

||| Represents a typed Query tree.
|||
||| @s The schema of attributes for the given query
data Query : (s:Schema) -> Type where
  ||| Represents raw data as a Query.
  |||
  ||| @rs The list of rows making up the table.
  Table   : (rs:List (Row s)) -> Query s
  ||| Represents the union of two queries.
  Union   : Query s -> Query s -> Query s
  ||| Represents the set difference of two queries.
  Diff    : Query s -> Query s -> Query s
  ||| Represents the cartesian product of two queries.
  ||| N.B. Currently not safe because Disjoint is not implemented.
  Product : Query s -> Query s' -> { auto p : Disjoint s s' } -> Query (s ++ s')
  ||| Represents the projection of a new schema onto a Query.
  Projection : (f:String -> Maybe String) -> Query s -> Query (projectedSchema f s)
  ||| Represents selection on a Query using the given expression.
  Select  : Expr s Bool -> Query s -> Query s

infixl 5 :++:
||| Append two Rows while preserving the type annotations.
(:++:) : Row s -> Row s' -> Row (s ++ s')
(:++:) [] ys      = ys
(:++:) (x::xs) ys = x :: (xs :++: ys)

||| Get the value of an attribute given a proof that the
||| attribute exists
lookupVal : (Row s) -> (nm:String) -> (p : (map cast s) `ContainsKey` nm) -> lookup' s p
lookupVal (x::xs) nm Here       = x
lookupVal (x::xs) nm (There s') = lookupVal xs nm s'

||| Evaluates an Expr in the context of a row.
evalExpr : Expr s t -> Row s -> t
evalExpr (Lit s x)      _ = x
evalExpr (x + y)        r = evalExpr x r + evalExpr y r
evalExpr ((^) s nm {p}) r = lookupVal r nm p
evalExpr (x == y)       r = evalExpr x r == evalExpr y r

||| Evaluates a Query, returning a List of Rows.
partial
eval : Query s -> List (Row s)
eval (Table xs) = xs
eval (Union x y) = eval x ++ eval y
eval (Diff x y) = (eval x) \\ (eval y)
eval (Product x y) = [ x' ++ y' | x' <- eval x, y' <- eval y ]
eval (Projection f x) = map (project f) (eval x)
eval (Select e x) = filter (evalExpr e) (eval x)
