module PowerOfPi
import Data.So
%default total

data U : Type where
  UString : U
  UInt    : U
  UBool   : U

el : U -> Type
el UString = String
el UInt    = Int
el UBool   = Bool

Attribute : Type
Attribute = Pair String U

Schema : Type
Schema = List Attribute

Cars : Schema
Cars = [ ("Model",UString) , ("Time",UString) , ("Wet",UBool) ]

data Row : Schema -> Type where
    Nil  : Row []
    (::) : (el u) -> Row s -> Row ((name,u)::s)

zonda : Row Cars
zonda = ["Pagani Zonda C12 F","1:18.4",False]

disjoint : Schema -> Schema -> Bool
sub      : Schema -> Schema -> Bool

Expr : Schema -> U -> Type

data RA : Schema -> Type where
  -- Read    : ?handleS -> RA s
  Union   : RA s -> RA s -> RA s
  Diff    : RA s -> RA s -> RA s
  Product : RA s -> RA s' -> { auto p : So (disjoint s s') } -> RA (s ++ s')
  Project : (s':Schema) -> RA s -> { auto p : So (sub s' s) } -> RA s'
  Select  : Expr s UBool -> RA s -> RA s

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

lookup' : (ps:Schema) -> ps `ContainsKey` k -> U
lookup' ((k, v) :: ps) Here = v
lookup' ((k', v) :: ps) (There x) = lookup' ps x

data Expr : Schema -> U -> Type where
  (^) : (s:Schema) -> (nm:String) -> { auto p : s `ContainsKey` nm } -> Expr s (lookup' s p)

infixl 5 ^

modelExpr : Expr Cars UString
modelExpr = Cars ^ "Model"















