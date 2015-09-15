module Database.PowerOfPi.Abstract.Expr

import public Database.PowerOfPi.Abstract.Types
%default total

||| Looks up the Type associated with 'nm'
|||
||| @ps The schema to check
||| @k  The name to look for
lookupType : (ps:Schema) -> (map cast ps) `ContainsKey` k -> Type
lookupType (k:::v :: ps) Here = v
lookupType (k':::v :: ps) (There x) = lookupType ps x

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
  (+) : Num t  => Expr s t -> Expr s t -> Expr s t
  (-) : Num t  => Expr s t -> Expr s t -> Expr s t
  (/) : Expr s Float -> Expr s Float -> Expr s Float
  (*) : Num t  => Expr s t -> Expr s t -> Expr s t
  (==): Eq t   => Expr s t -> Expr s t -> Expr s Bool
  (/=): Eq t   => Expr s t -> Expr s t -> Expr s Bool
  Lit : Show t => (val:t) -> Expr s t
  Couple : Expr s t -> Expr s t' -> Expr s $ Pair t t'
  PureFn : (a -> b) -> Expr s a -> Expr s b

infixl 9 ^

getType : Expr _ t -> Type
getType {t} _ = t
-- TODO : Check whether getType is used anywhere
