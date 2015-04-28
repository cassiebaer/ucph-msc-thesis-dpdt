module PowerOfPi
import Data.So
%default total

data Ty : Type where
  TString : Ty
  TInt    : Ty
  TBool   : Ty

InterpTy : Ty -> Type
InterpTy TInt    = Int
InterpTy TBool   = Bool
InterpTy TString = String

Attribute : Type
Attribute = Pair String Ty

infix 8 :::
(:::) : String -> Ty -> Attribute
(:::) n t = (n,t)

data AttrEq : Attribute -> Attribute -> Type where
  AttrRefl : { auto strEq : n1 = n2 } -> AttrEq (n1 ::: t) (n2 ::: t)

Schema : Type
Schema = List Attribute

Cars : Schema
Cars = ["Model":::TString, "Time":::TString, "Wet":::TBool]

data Row : Schema -> Type where
    Nil  : Row []
    (::) : (InterpTy t) -> Row s -> Row (name:::t::s)

zonda : Row Cars
zonda = ["Pagani Zonda C12 F","1:18.4",False]

disjoint : Schema -> Schema -> Bool
sub      : Schema -> Schema -> Bool

Expr : Schema -> Ty -> Type

data Query : Schema -> Type where
  Table   : Query s
  Union   : Query s -> Query s -> Query s
  Diff    : Query s -> Query s -> Query s
  Product : Query s -> Query s' -> { auto p : So (disjoint s s') } -> Query (s ++ s')
  Project : (s':Schema) -> Query s -> { auto p : So (sub s' s) } -> Query s'
  Select  : Expr s TBool -> Query s -> Query s

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

lookup' : (ps:Schema) -> ps `ContainsKey` k -> Ty
lookup' ((k, v) :: ps) Here = v
lookup' ((k', v) :: ps) (There x) = lookup' ps x

isNum : Ty -> Bool
isNum TInt    = True
isNum TBool   = False
isNum TString = False

infixl 5 ^
data Expr : Schema -> Ty -> Type where
  (^) : (s:Schema) -> (nm:String) -> { auto p : s `ContainsKey` nm } -> Expr s (lookup' s p)
  (+) : { auto p : So (isNum t) } -> Expr s t -> Expr s t -> Expr s t

modelExpr : Expr Cars TString
modelExpr = Cars ^ "Model"

