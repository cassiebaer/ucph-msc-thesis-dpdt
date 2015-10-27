module Database.DPDT.Types

import Data.So

namespace PINQ

  ||| Type alias for `Double`.
  Eps : Type
  Eps = Double

  ||| Type alias for `List`.
  Bag : Type -> Type
  Bag = List

  ||| Type alias for `Bag`.
  IQueryable : Type -> Type
  IQueryable = Bag

  ||| Type alias for `Bag`.
  IEnumerable : Type -> Type
  IEnumerable = Bag

  ||| Type alias for `Bag (k,v)`.
  IGrouping : Type -> Type -> Type
  IGrouping k v = Bag (Pair k v)

namespace Schemas

  infix 8 :::
  data Attribute : Type where
    (:::) : String -> Type -> Attribute

  getName : Attribute -> String
  getName (n ::: t) = n

  getType : Attribute -> Type
  getType (n ::: t) = t

  data AttrEq : Attribute -> Attribute -> Type where
    AttrRefl : { colMatch : So (n1 == n2) } -> AttrEq (n1 ::: t) (n2 ::: t)
                         -- ^ TODO : Can we use Decidable Equality instead?

  data Schema : Type where
    Nil  : Schema
    (::) : (a:Attribute) -> (s:Schema) -> Schema

  data SchemaEq : Schema -> Schema -> Type where
    NilEq  : SchemaEq [] []
    ConsEq : AttrEq a1 a2 -> SchemaEq s1 s2 -> SchemaEq (a1 :: s1) (a2 :: s2)

  colNames : Schema -> List String
  colNames [] = []
  colNames ((n ::: t) :: s) = n :: colNames s

  data HasType : Schema -> String -> Type -> Type where
    Here  : So (n == n')  -> HasType (n' ::: t  :: s) n t
    There : HasType s n t -> HasType (n' ::: t' :: s) n t

  nilSchemaEmpty : HasType [] c t -> Void
  nilSchemaEmpty (Here _)  impossible
  nilSchemaEmpty (There _) impossible

  --decHasType : (s:Schema) -> (n:String) -> (t:Type) -> Dec (HasType s n t)
  --decHasType [] n t'      = No nilSchemaEmpty
  --decHasType ((n':::t')::s) n t with (decEq n' n, decTypeEq t' t, decHasType s n t)
    --decHasType ((n':::t')::s) n t | (Yes p, Yes p', _) = ?decHasType_rhs_1
    --decHasType ((n':::t')::s) n t | (_, _, _) = ?decHasType_rhs_1

