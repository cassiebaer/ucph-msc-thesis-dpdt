module Database.PowerOfPi.Idris.Expr

import Database.PowerOfPi.Abstract
import Database.PowerOfPi.Idris.Types

||| Get the value of an attribute given a proof that the
||| attribute exists
lookupVal : (Row s) -> (nm:String) -> (p : (map cast s) `ContainsKey` nm) -> lookupType s p
lookupVal (x::xs) nm Here       = x
lookupVal (x::xs) nm (There s') = lookupVal xs nm s'

||| Evaluates an Expr in the context of a row.
eval : Expr s t -> Row s -> t
eval (Lit x)        _ = x
eval (x + y)        r = eval x r + eval y r
eval ((^) _ nm {p}) r = lookupVal r nm p
eval (x == y)       r = eval x r == eval y r
eval (PureFn f x)   r = f (eval x r)
eval (Couple x y)   r = (eval x r, eval y r)

