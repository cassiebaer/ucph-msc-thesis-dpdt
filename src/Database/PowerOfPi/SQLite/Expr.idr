module Database.PowerOfPi.SQLite.Expr

import Database.PowerOfPi.Abstract.Expr

||| Evaluates an Expr to an SQLite expression
%assert_total
eval : Expr s t -> String
eval (Lit x)      = show x
eval (x + y)      = eval x ++ " + "  ++ eval y
eval ((^) _ nm)   = nm 
eval (x == y)     = eval x ++ " == " ++ eval y
eval (PureFn f x) = "TODO"

