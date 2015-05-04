module Database.PowerOfPi.Expr
import Database.PowerOfPi.Types
%default total

||| Looks up the Type associated with 'nm'
|||
||| @ps The schema to check
||| @k  The name to look for
lookupType : (ps:Schema) -> (map cast ps) `ContainsKey` k -> Type
lookupType (k:::v :: ps) Here = v
lookupType (k':::v :: ps) (There x) = lookupType ps x

||| Get the value of an attribute given a proof that the
||| attribute exists
lookupVal : (Row s) -> (nm:String) -> (p : (map cast s) `ContainsKey` nm) -> lookupType s p
lookupVal (x::xs) nm Here       = x
lookupVal (x::xs) nm (There s') = lookupVal xs nm s'

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
  (+) : Num t => Expr s t -> Expr s t -> Expr s t
  (==): Eq t  => Expr s t -> Expr s t -> Expr s Bool
  Lit : {t:Type} -> (val:t) -> Expr s t
  PureFn : (a -> b) -> Expr s a -> Expr s b
  RowFn  : (Row s -> a) -> Expr s a

infixl 5 ^

||| Evaluates an Expr in the context of a row.
evalExpr : Expr s t -> Row s -> t
evalExpr (Lit x)        _ = x
evalExpr (x + y)        r = evalExpr x r + evalExpr y r
evalExpr ((^) _ nm {p}) r = lookupVal r nm p
evalExpr (x == y)       r = evalExpr x r == evalExpr y r
evalExpr (PureFn f x)   r = f (evalExpr x r)
evalExpr (RowFn f)      r = f r

