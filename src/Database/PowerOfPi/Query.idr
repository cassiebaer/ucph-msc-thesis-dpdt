module Database.PowerOfPi.Query
import Data.So
import Database.PowerOfPi.Types
import Database.PowerOfPi.Expr
%default total

mapMaybeOverName : (f:String -> Maybe String) -> (s:Schema) -> Schema
mapMaybeOverName f [] = []
mapMaybeOverName f ((n:::t)::xs) with (f n, mapMaybeOverName f xs)
  mapMaybeOverName f ((n:::t)::xs) | (Nothing, ys) = ys
  mapMaybeOverName f ((n:::t)::xs) | (Just n', ys) = (n':::t) :: ys

Person : Schema
Person = [ "Name" ::: String , "Age" ::: Int , "Food" ::: String ]

foo : Schema
foo = mapMaybeOverName bar Person
  where bar : String -> Maybe String
        bar "Name" = Just "Nickname"
        bar "Age"  = Just "Age"
        bar x = Nothing

rowProj : (f:String -> Maybe String) -> Row s -> (t : Schema ** Row t)
rowProj f []      {s = []}          = ([] ** [])
rowProj f (x::xs) {s=((n:::t)::as)} with (f n, rowProj f xs)
  rowProj f (x::xs) {s=((n:::t)::as)} | (Nothing, (s' ** xs')) = (s' ** xs')
  rowProj f (x::xs) {s=((n:::t)::as)} | (Just n', (s' ** xs')) = ((n:::t)::s' ** x :: xs')

casper : Row Person
casper = [ "Casper" , 25, "Bruschetta" ]

whatIsMyName : Row [ "Name" ::: String ]
whatIsMyName = getProof $ rowProj proj casper
  where proj : String -> Maybe String
        proj "Name" = Just "Name"
        proj _      = Nothing

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
  Project : (f:String -> Maybe String) -> Query s -> Query (mapMaybeOverName f s)
  ||| Represents selection on a Query using the given expression.
  Select  : Expr s Bool -> Query s -> Query s

infixl 5 :++:
||| Append two Rows while preserving the type annotations.
(:++:) : Row s -> Row s' -> Row (s ++ s')
(:++:) [] ys      = ys
(:++:) (x::xs) ys = x :: (xs :++: ys)

||| Evaluates a Query, returning a List of Rows.
partial
eval : Query s -> List (Row s)
eval (Table xs) = xs
eval (Union x y) = eval x ++ eval y
eval (Diff x y) = (eval x) \\ (eval y)
eval (Product x y) = [ x' :++: y' | x' <- eval x, y' <- eval y ]
eval (Project f x) = let test = map (getProof . rowProj f) (eval x)
                      in ?fooooikkkk
eval (Select x y) = ?eval_rhs_6

