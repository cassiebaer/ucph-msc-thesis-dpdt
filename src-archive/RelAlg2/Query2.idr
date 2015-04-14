module Query2
import Data.So
%default total

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------

mutual
  data QueryExpr : Type where
      QTable : QueryExpr
      QUn    : QUnary  -> QueryExpr -> QueryExpr
      QBin   : QBinary -> QueryExpr -> QueryExpr -> QueryExpr

  data QUnary : Type where
      Projection : QUnary
      Selection  : QUnary
      Rename     : QUnary

  data QBinary : Type where
      Union        : QBinary
      Intersection : QBinary
      Difference   : QBinary
      Join         : QBinary

------------------------------------------------------------------------------
-- Add Types
------------------------------------------------------------------------------

Attribute : Type
Attribute = String

Schema : Type
Schema = List Attribute

isSubsetOf : (Ord a) => List a -> List a -> Bool
isSubsetOf xs ys = all (\x => elem x ys) xs

data TypedQueryExpr : Schema -> Type where
  TypedQuery : QueryExpr -> (s:Schema) -> TypedQueryExpr s

------------------------------------------------------------------------------
-- Embed language
------------------------------------------------------------------------------

-- Unary
project : (s:Schema) -> TypedQueryExpr t -> {auto p : So (s `isSubsetOf` t)} -> TypedQueryExpr s
select : ?filter -> TypedQueryExpr s -> TypedQueryExpr s
rename : (f: Attribute -> Maybe Attribute) -> TypedQueryExpr s -> TypedQueryExpr (mapMaybe f s)

-- Binary
union : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
intersection : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
difference : TypedQueryExpr s -> TypedQueryExpr s -> TypedQueryExpr s
join : TypedQueryExpr s -> TypedQueryExpr t -> TypedQueryExpr (s ++ t)

------------------------------------------------------------------------------


