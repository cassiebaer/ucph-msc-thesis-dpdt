module Database.PowerOfPi.Query
import Data.So
import Database.PowerOfPi.Types
import Database.PowerOfPi.Expr
%default total

sub : Schema -> Schema -> Bool

data Query : Schema -> Type where
  Table   : List (Row s) -> Query s
  Union   : Query s -> Query s -> Query s
  Diff    : Query s -> Query s -> Query s
  Product : Query s -> Query s' -> { auto p : Disjoint s s' } -> Query (s ++ s')
  Project : (s':Schema) -> Query s -> { auto p : So (sub s' s) } -> Query s'
  Select  : Expr s Bool -> Query s -> Query s

infixl 5 :++:
(:++:) : Row s -> Row s' -> Row (s ++ s')
(:++:) [] ys      = ys
(:++:) (x::xs) ys = x :: (xs :++: ys)

instance Eq (Row s) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && xs == ys

eval : Query s -> List (Row s)
eval (Table xs) = xs
eval (Union x y) = eval x ++ eval y
eval (Diff x y) = (eval x) \\ (eval y)
eval (Product x y) = [ x' :++: y' | x' <- eval x, y' <- eval y ]
eval (Project s x) = ?eval_rhs_5
eval (Select x y) = ?eval_rhs_6
