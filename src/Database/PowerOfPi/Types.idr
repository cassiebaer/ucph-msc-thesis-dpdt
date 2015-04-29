module Database.PowerOfPi.Types
%default total

namespace Attribute

  data Attribute : Type where
    (:::) : String -> Type -> Attribute
  infix 8 :::

  instance Eq Attribute where
    (==) (n:::t) (n':::t') = n == n'

  instance Cast Attribute (Pair String Type) where
    cast (n ::: t) = (n,t)

namespace Schema

  Schema : Type
  Schema = List Attribute

  data Row : Schema -> Type where
    Nil  : Row []
    (::) : Eq t => t -> Row s -> Row (name:::t::s)

namespace Proofs

  -- TODO : Fix disjoint!
  data Disjoint : Schema -> Schema -> Type where
    FakeDisjoint : Disjoint xs ys
