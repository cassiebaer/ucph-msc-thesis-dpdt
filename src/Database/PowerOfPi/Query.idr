module Database.PowerOfPi.Query
import Data.So
import Database.PowerOfPi.Types
import Database.PowerOfPi.Expr
%default total

-- TODO : implement sub; preferably with a constructive proof
sub : Schema -> Schema -> Bool

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
  Project : (s':Schema) -> Query s -> { auto p : So (sub s' s) } -> Query s'
  ||| Represents selection on a Query using the given expression.
  Select  : Expr s Bool -> Query s -> Query s

infixl 5 :++:
||| Append two Rows while preserving the type annotations.
(:++:) : Row s -> Row s' -> Row (s ++ s')
(:++:) [] ys      = ys
(:++:) (x::xs) ys = x :: (xs :++: ys)

||| Evaluates a Query, returning a List of Rows.
eval : Query s -> List (Row s)
eval (Table xs) = xs
eval (Union x y) = eval x ++ eval y
eval (Diff x y) = (eval x) \\ (eval y)
eval (Product x y) = [ x' :++: y' | x' <- eval x, y' <- eval y ]
eval (Project s x) = ?eval_rhs_5
eval (Select x y) = ?eval_rhs_6

