module PowerOfPi
import Data.So
%default total

Attribute : Type
Attribute = Pair String Type

infix 8 :::
(:::) : String -> Type -> Attribute
(:::) n t = (n,t)

data AttrEq : Attribute -> Attribute -> Type where
  AttrRefl : { auto strEq : n1 = n2 } -> AttrEq (n1 ::: t) (n2 ::: t)

Schema : Type
Schema = List Attribute

Cars : Schema
Cars = ["Model":::String, "Time":::String, "Wet":::Bool]

data Row : Schema -> Type where
    Nil  : Row []
    (::) : t -> Row s -> Row (name:::t::s)

zonda : Row Cars
zonda = ["Pagani Zonda C12 F","1:18.4",False]

disjoint : Schema -> Schema -> Bool
sub      : Schema -> Schema -> Bool

Expr : Schema -> Type -> Type

data Query : Schema -> Type where
  Table   : Query s
  Union   : Query s -> Query s -> Query s
  Diff    : Query s -> Query s -> Query s
  Product : Query s -> Query s' -> { auto p : So (disjoint s s') } -> Query (s ++ s')
  Project : (s':Schema) -> Query s -> { auto p : So (sub s' s) } -> Query s'
  Select  : Expr s Bool -> Query s -> Query s

------------------------------------------------------------------------------
-- ContainsKey Decidability
------------------------------------------------------------------------------

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

lookup' : (ps:Schema) -> ps `ContainsKey` k -> Type
lookup' ((k, v) :: ps) Here = v
lookup' ((k', v) :: ps) (There x) = lookup' ps x

infixl 5 ^
data Expr : Schema -> Type -> Type where
  (^) : (s:Schema) -> (nm:String) -> { auto p : s `ContainsKey` nm } -> Expr s (lookup' s p)
  (+) : Expr s t -> Expr s t -> Expr s t

modelExpr : Expr Cars String
modelExpr = Cars ^ "Model"

