module Data.Dictionary

%default total

||| A `Dictionary` is just a `List` of key/value pairs
Dictionary : Type -> Type -> Type
Dictionary k v = List (k,v)

||| Builds a `Dictionary k v` from the given `List` of key/value pairs.
fromList : List (k,v) -> Dictionary k v
fromList = id

||| Returns a `List` of the key/value pairs in a `Dictionary`.
toList : Dictionary k v -> List (k,v)
toList = id

||| Inserts the key+element pair into the Dictionary,
||| using the given function to combine the old and new pairs.
|||
||| The arguments to the combining function are the old and then
||| the new value, respectively.
insertWith : Eq k => k -> v -> (v -> v -> v) -> Dictionary k v -> Dictionary k v
insertWith k v f []            = [(k,v)]
insertWith k v f ((k',v')::ps) = if k == k'
                                    then (k',f v' v)::ps
                                    else (k',v')::insertWith k v f ps

||| Inserts the key+element pair into the Dictionary,
||| overwriting any previously existing value at that key.
insert : Eq k => k -> v -> Dictionary k v -> Dictionary k v
insert k v = insertWith k v (\_,new => new)
--insert k v []            = [(k,v)]
--insert k v ((k',v')::ps) = if k == k'
                              --then (k',v)::ps
                              --else (k',v')::insert k v ps

||| Looks up the value associated with a given key, and
||| if it doesn't exist, returns the provided default value.
lookupWithDefault : Eq k => k -> v -> Dictionary k v -> v
lookupWithDefault k def []            = def
lookupWithDefault k def ((k',v')::ps) = if k == k'
                                           then v'
                                           else lookupWithDefault k def ps

