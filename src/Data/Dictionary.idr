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
insertWith : Eq k => (v -> v -> v) -> k -> v -> Dictionary k v -> Dictionary k v
insertWith f k v []            = [(k,v)]
insertWith f k v ((k',v')::ps) = if k == k'
                                    then (k',f v' v)::ps
                                    else (k',v')::insertWith f k v ps

||| Inserts the key+element pair into the Dictionary,
||| overwriting any previously existing value at that key.
insert : Eq k => k -> v -> Dictionary k v -> Dictionary k v
insert k v = insertWith (\_,new => new) k v
--insert k v []            = [(k,v)]
--insert k v ((k',v')::ps) = if k == k'
                              --then (k',v)::ps
                              --else (k',v')::insert k v ps

||| Looks up the value associated with a given key, and
||| if it doesn't exist, returns the provided default value.
lookupWithDefault : Eq k => v -> k -> Dictionary k v -> v
lookupWithDefault def k []            = def
lookupWithDefault def k ((k',v')::ps) = if k == k'
                                           then v'
                                           else lookupWithDefault def k ps

