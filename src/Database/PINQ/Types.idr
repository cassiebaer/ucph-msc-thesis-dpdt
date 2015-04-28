module Database.PINQ.Types

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
