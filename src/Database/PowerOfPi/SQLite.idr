module Database.PowerOfPi.SQLite

import public Database.PowerOfPi

||| Evaluates an Expr to an SQLite expression
eval : Expr s t -> String
eval (Lit x)      = show x
eval (x + y)      = "(" ++ eval x ++ ") + ("  ++ eval y ++ ")"
eval (x - y)      = "(" ++ eval x ++ ") - ("  ++ eval y ++ ")"
eval (x / y)      = "(" ++ eval x ++ ") / ("  ++ eval y ++ ")"
eval (x * y)      = "(" ++ eval x ++ ") * ("  ++ eval y ++ ")"
eval ((^) _ nm)   = nm
eval (x == y)     = "(" ++ eval x ++ ") == (" ++ eval y ++ ")"
eval (x /= y)     = "(" ++ eval x ++ ") <> (" ++ eval y ++ ")"

----------------------------------------------------------------

namespace Query

  SQLiteTable : Schema -> Type
  SQLiteTable _ = String

  Query : Schema -> Type
  Query = Query SQLiteTable

  commasBetween : List String -> String
  commasBetween xs = concat $ intersperse ", " xs

  ||| Evaluates a Query, returning a List of Rows.
  %assert_total
  eval : Query SQLiteTable s -> String
  eval (Table xs)       = xs
  eval (Union x y)      = "(" ++ eval x ++ ") Union (" ++ eval y ++ ")"
  eval (Diff x y)       = "(" ++ eval x ++ ") Intersect (" ++ eval y ++ ")"
  eval (Product x y)    = "(" ++ eval x ++ ") , (" ++ eval y ++ ")"
  eval (Projection f x {s}) = "Select " ++ commasBetween cols ++ " From (" ++ eval x ++ ")" where
    maybes : List (Maybe String)
    maybes = map f $ getNames s
    cols : List String
    cols = catMaybes maybes
  eval (Select e x {s}) = "Select " ++ cols ++ " From " ++ eval x ++ " Where " ++ eval e where
    cols : String
    cols = commasBetween $ getNames s
