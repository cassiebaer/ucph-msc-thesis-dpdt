module PowerOfPi
import Data.So
%default total

data Attribute : Type where
  (:::) : String -> Type -> Attribute
infix 8 :::

instance Eq Attribute where
  (==) (n:::t) (n':::t') = n == n'

instance Cast Attribute (Pair String Type) where
    cast (n ::: t) = (n,t)

--data AttrEq : Attribute -> Attribute -> Type where
--  AttrRefl : { auto strEq : n1 = n2 } -> AttrEq (n1 ::: t) (n2 ::: t)

Schema : Type
Schema = List Attribute

data Row : Schema -> Type where
    Nil  : Row []
    (::) : Eq t => t -> Row s -> Row (name:::t::s)

disjoint : Schema -> Schema -> Bool
disjoint [] s2 = True
disjoint (x :: xs) s2 = not (elem x s2) && disjoint xs s2

sub      : Schema -> Schema -> Bool

-- TODO : fix Disjoint!
data Disjoint : Schema -> Schema -> Type where
  FakeDisjoint : Disjoint xs ys
  --Disjoint1 : (s1:Schema) -> (s2:Schema) -> { auto p : So (disjoint s1 s2) } -> Disjoint s1 s2
  --Disjoint2 : Disjoint (x::xs) ys -> Disjoint xs ys

Expr : Schema -> Type -> Type

data Query : Schema -> Type where
  Table   : List (Row s) -> Query s
  Union   : Query s -> Query s -> Query s
  Diff    : Query s -> Query s -> Query s
  Product : Query s -> Query s' -> { auto p : Disjoint s s' } -> Query (s ++ s')
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

lookup' : (ps:Schema) -> (map cast ps) `ContainsKey` k -> Type
lookup' (k:::v :: ps) Here = v
lookup' (k':::v :: ps) (There x) = lookup' ps x


infixl 5 ^
data Expr : Schema -> Type -> Type where
  (^) : (s:Schema) -> (nm:String) -> { auto p : (map cast s) `ContainsKey` nm } -> Expr s (lookup' s p)
  (+) : Num t => Expr s t -> Expr s t -> Expr s t

infixl 5 :++:
(:++:) : Row s -> Row s' -> Row (s ++ s')
(:++:) [] ys      = ys
(:++:) (x::xs) ys = x :: (xs :++: ys)

instance Eq (Row s) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && xs == ys

eval : Query s -> List (Row s)
eval (Table xs) = xs
eval (Union x y) = eval x ++ eval y
eval (Diff x y) = (eval x) \\ (eval y)
eval (Product x y) = [ x' :++: y' | x' <- eval x, y' <- eval y ]
eval (Project s x) = ?eval_rhs_5
eval (Select x y) = ?eval_rhs_6

