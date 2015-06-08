module Database.PowerOfPi.Types
%default total

namespace Attribute

  ||| Represents a table attribute.
  data Attribute : Type where
    (:::) : String -> Type -> Attribute
  infix 8 :::

  -- TODO : Implement equality check for Type as well?
  ||| Equality is currently only based on names!
  instance Eq Attribute where
    (==) (n:::t) (n':::t') = n == n'

  instance Cast Attribute (Pair String Type) where
    cast (n ::: t) = (n,t)

namespace Schema

  ||| Represents a table.
  Schema : Type
  Schema = List Attribute

  getNames : Schema -> List String
  getNames s = map (fst . cast) s

  projectedSchema : (f:String -> Maybe String) -> Schema -> Schema
  projectedSchema f [] = []
  projectedSchema f (n:::t::as) = maybe (projectedSchema f as) (\n' => n':::t::projectedSchema f as) (f n)

namespace Proofs

  -- TODO : Fix disjoint!
  ||| A proof that two schemas are disjoint.
  ||| N.B. This is currently not implemented.
  data Disjoint : Schema -> Schema -> Type where
    FakeDisjoint : Disjoint xs ys

  -- TODO : Contribute ContainsKey to Idris HEAD
  -- https://github.com/idris-lang/Idris-dev/pull/2202

  ||| A proof that an association list contains the given key.
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

