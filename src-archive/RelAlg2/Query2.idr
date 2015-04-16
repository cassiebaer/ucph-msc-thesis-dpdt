module Query2
import Data.So
%default total

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

mutual
  data QueryExpr : Type where
      QTable : String  -> List (Attribute,Type) -> QueryExpr
      QUn    : QUnary  -> QueryExpr -> QueryExpr
      QBin   : QBinary -> QueryExpr -> QueryExpr -> QueryExpr
  %name QueryExpr q

  data QUnary : Type where
      Projection : QUnary
      Selection  : QUnary
      Rename     : Assoc -> QUnary

  data QBinary : Type where
      Union        : QBinary
      Intersection : QBinary
      Difference   : QBinary
      Join         : QBinary

  -- TODO: [("age",Nat),("name",String)]

  Attribute : Type
  Attribute = String

  Assoc : Type
  Assoc = List (Attribute,Attribute)

------------------------------------------------------------------------------
-- Add Types
------------------------------------------------------------------------------

Schema : Type
Schema = List Attribute

isSubsetOf : (Eq a) => List a -> List a -> Bool
isSubsetOf xs ys = all (\x => elem x ys) xs

data TypedQueryExpr : (s:Schema) -> Type where
  TypedQuery : QueryExpr -> TypedQueryExpr s

map : (QueryExpr -> QueryExpr) -> TypedQueryExpr s -> TypedQueryExpr t
map f (TypedQuery q) = TypedQuery (f q)

------------------------------------------------------------------------------
-- Embed language
------------------------------------------------------------------------------

table : String -> (s:List (String,Type)) -> TypedQueryExpr (map fst s)
table name schema = TypedQuery (QTable name schema)

namespace example
  myTable : TypedQueryExpr ["name","age"]
  myTable = table "MyTable" [("name",String),("age",Int)]

-- Unary

project : (s:Schema) -> TypedQueryExpr t -> {auto p : So (s `isSubsetOf` t)} -> TypedQueryExpr s
project s t = map (QUn Projection) t

select : ?filter -> TypedQueryExpr s -> TypedQueryExpr s
select f t = map f t

rename : (r:Assoc) -> TypedQueryExpr s -> TypedQueryExpr (mapMaybe (flip lookup r) s)
rename r t = map (QUn (Rename r)) t

-- Binary

union : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
union (TypedQuery q) (TypedQuery q1) = TypedQuery (QBin Union q q1)

intersection : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
intersection (TypedQuery q) (TypedQuery q1) = TypedQuery (QBin Intersection q q1)

difference : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
difference (TypedQuery q) (TypedQuery q1) = TypedQuery (QBin Difference q q1)

join : TypedQueryExpr s -> TypedQueryExpr t -> TypedQueryExpr (s ++ t)
join (TypedQuery q) (TypedQuery q1) = TypedQuery (QBin Join q q1)

------------------------------------------------------------------------------


