module Query2
import Data.So
%default total

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

mutual
  data QueryExpr : Type where
      QTable : String  -> QueryExpr
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

  Attribute : Type
  Attribute = String

  Assoc : Type
  Assoc = List (Attribute,Attribute)

------------------------------------------------------------------------------
-- Add Types
------------------------------------------------------------------------------

Schema : Type
Schema = List Attribute

isSubsetOf : (Ord a) => List a -> List a -> Bool
isSubsetOf xs ys = all (\x => elem x ys) xs

data TypedQueryExpr : Schema -> Type where
  TypedQuery : (s:Schema) -> QueryExpr -> TypedQueryExpr s

map : (QueryExpr -> QueryExpr) -> TypedQueryExpr s -> TypedQueryExpr s
map f (TypedQuery s q) = TypedQuery s (f q)

------------------------------------------------------------------------------
-- Embed language
------------------------------------------------------------------------------

table : String -> (s:Schema) -> TypedQueryExpr s
table name schema = TypedQuery schema (QTable name)

-- Unary

project : (s:Schema) -> TypedQueryExpr t -> {auto p : So (s `isSubsetOf` t)} -> TypedQueryExpr s
project s (TypedQuery t q) = TypedQuery s (QUn Projection q)

select : ?filter -> TypedQueryExpr s -> TypedQueryExpr s
select f t = map f t

rename : (r:Assoc) -> TypedQueryExpr s -> TypedQueryExpr (mapMaybe (flip lookup r) s)
rename r (TypedQuery s q) = TypedQuery (mapMaybe (flip lookup r) s) (QUn (Rename r) q)
-- not quite working ^^

-- Binary
union : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
intersection : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
difference : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
join : TypedQueryExpr s -> TypedQueryExpr t -> TypedQueryExpr (s ++ t)

------------------------------------------------------------------------------


