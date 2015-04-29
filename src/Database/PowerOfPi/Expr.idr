module Database.PowerOfPi.Expr
import Database.PowerOfPi.Types
%default total

-- TODO : Contribute ContainsKey to Idris HEAD
-- https://github.com/idris-lang/Idris-dev/pull/2202

data ContainsKey : List (k,v) -> k -> Type where
  Here  : ContainsKey ((k,v)::ps) k
  There : ContainsKey ps k -> ContainsKey ((k',v)::ps) k

instance Uninhabited (ContainsKey [] k) where
    uninhabited Here      impossible
    uninhabited (There p) impossible

decContainsKey : DecEq 'k => (ps:List ('k,'v)) -> (k:'k) -> Dec (ps `ContainsKey` k)
decContainsKey [] k = No absurd
decContainsKey ((a, b) :: ps) k with (decEq a k)
  decContainsKey ((k, b) :: ps) k | (Yes Refl) = Yes Here
  decContainsKey ((a, b) :: ps) k | (No notHere) with (ps `decContainsKey` k)
    decContainsKey ((a, b) :: ps) k | (No notHere) | (Yes prf) = Yes (There prf)
    decContainsKey ((a, b) :: ps) k | (No notHere) | (No notThere) = No (mkNo notHere notThere)
      where
        mkNo : ((k' = k) -> Void) ->
               (ps `ContainsKey` k -> Void) ->
               ((k',v')::ps) `ContainsKey` k -> Void
        mkNo f g Here = f Refl
        mkNo f g (There x) = g x


lookup' : (ps:Schema) -> (map cast ps) `ContainsKey` k -> Type
lookup' (k:::v :: ps) Here = v
lookup' (k':::v :: ps) (There x) = lookup' ps x

infixl 5 ^
data Expr : Schema -> Type -> Type where
  (^) : (s:Schema) -> (nm:String) -> { auto p : (map cast s) `ContainsKey` nm } -> Expr s (lookup' s p)
  (+) : Num t => Expr s t -> Expr s t -> Expr s t
