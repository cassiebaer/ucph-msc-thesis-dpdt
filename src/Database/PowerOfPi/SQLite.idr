module Database.PowerOfPi.SQLite

import public Database.PowerOfPi

%default total

namespace Expr
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
  eval (Couple x y) = ""
  eval (PureFn f x) = ""

----------------------------------------------------------------

mutual

  namespace Query
  
    SQLiteTable : Schema -> Type
    SQLiteTable _ = String
  
    Query : Schema -> Type
    Query = Query SQLiteTable

    commasBetween : List String -> String
    commasBetween xs = concat $ intersperse ", " xs
    
    ||| Extracts the column name of the key of a group by query string 
    groupByKeyColName : String -> String
    groupByKeyColName g = let afterLastLeftParan = fromMaybe "" $ last' $ split (== '(') g
                          in  fst $ break (== ')') afterLastLeftParan -- removes the ending paran
                           
    ||| Evaluates a Query, returning a MySQL query string.
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
    eval (Select e x {s}) = "Select " ++ cols ++ " From (" ++ eval x ++ ") Where (" ++ eval e ++ ")" where
      cols : String
      cols = commasBetween $ getNames s
    eval (Lookup k g) = let gString = eval g
                            selectFromPart = "Select * From (" ++ gString ++ ") "
                            wherePart      = "Where " ++ groupByKeyColName gString ++ " == " ++ show k
                        in  selectFromPart ++ wherePart
  
  namespace Grouping
    
    Grouping : (Num k, Show k) => Schema -> (k:Type) -> Type
    Grouping = Grouping SQLiteTable

    ||| Evaluates a Grouping, returning a MySQL group by query string.
    ||| PS: The only expression e that will work is a lookup, and the compiler
    ||| will not complain if the user chooses to write other expressions.
    eval : Grouping SQLiteTable s k -> String
    eval (MkGrouping e q) = "Select * From (" ++ eval q ++ ") Group By (" ++ eval e ++ ")"
  
  namespace Partition
    ||| Intersperse a list of keys with " Or " in a string
    keysToOrs : Show k => List k -> String
    keysToOrs []      = ""
    keysToOrs (k::[]) = show k
    keysToOrs (k::ks) = show k ++ " Or "
  
    ||| Evaluates a Partitioning, returning a MySQL group my query string,
    ||| which also makes sure the resulting group only contains values from
    ||| the list of keys ks
    ||| PS: The expression e has the same limitations as in Grouping
    eval : Partitioning SQLiteTable s k -> String
    eval (MkPartitioning ks e q) = let keyCol = eval e
                                       selectFromPart = "Select * From (" ++ eval q ++ ") "
                                       wherePart      = "Where " ++ keyCol ++ " == (" ++ keysToOrs ks ++ ") "
                                       groupByPart    = "Group By (" ++ keyCol ++ ")"
                                   in  selectFromPart ++ wherePart ++ groupByPart
