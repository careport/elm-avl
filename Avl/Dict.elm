module Avl.Dict exposing
  ( size
  , empty
  , isEmpty
  , singleton
  , member
  , get
  , insert
  , remove
  , update
  , foldl
  , foldr
  , toList
  , keys
  , values
  , fromList
  , Dict )

{-| A dictionary mapping unique keys to values. The keys can have
any type, since the user is required to provide an explicit comparison
function with type `key -> key -> Order`.

The interface is otherwise very similar to the core `Dict` type.


# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, values, toList, fromList

# Transform
@docs foldl, foldr

-}

import Avl exposing (Cmp)
import Avl.Tree as Tree

{-| A dictionary from keys to values -}
type Dict k v = Dict (Tree.Node k v)

{-| The number of keys mapped by the dictionary. -}
size: Dict k v -> Int
size (Dict t) = Tree.size t

{-| `True` if the dictionary is empty (i.e., maps no keys),
False` otherwise. -}
isEmpty: Dict k v -> Bool
isEmpty d = (size d) == 0

{-| The empty dictionary. -}
empty: Dict k v
empty = Dict Tree.empty

{-| Creates a dictionary with one key-value mapping. -}
singleton: k -> v -> Dict k v
singleton key val =
  Dict (Tree.singleton key val)

{-| `True` if the key is in the dictionary, `False` otherwise. -}
member: Cmp k -> k -> Dict k v -> Bool
member cmp key (Dict t) =
  Tree.member cmp key t

{-| Returns the value associated with the key. If the key isn't in
the dictionary, `Nothing` is returned. -}
get: Cmp k -> k -> Dict k v -> Maybe v
get cmp key (Dict t) =
  Tree.get cmp key t

{-| Inserts a key-value mapping. If the key is already in the
dictionary, the mapping is updated with the given value. -}
insert: Cmp k -> k -> v -> Dict k v -> Dict k v
insert cmp key val (Dict t) =
  Dict (Tree.insert cmp key val t)

{-| Removes a key-value mapping. If the key is not in the dictionary,
then the dictionary is unchanged. -}
remove: Cmp k -> k -> Dict k v -> Dict k v
remove cmp key (Dict t) =
  Dict (Tree.remove cmp key t)

{-| Updates a key-value mapping using the given function. If the function
returns `Nothing`, then any existing mapping for the given key will be removed. -}
update: Cmp k -> k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update cmp key fn (Dict t) =
  Dict (Tree.update cmp key fn t)

{-| Folds the given function over the dictionary's key-value pairs,
starting with the lowest-valued key and ending with the highest-valued one. -}
foldl: (k -> v -> b -> b) -> b -> Dict k v -> b
foldl fn nil (Dict t) =
  Tree.foldl fn nil t

{-| Folds the given function over the dictionary's key-value pairs,
starting with the highest-valued key and ending with the lowest-valued one. -}
foldr: (k -> v -> b -> b) -> b -> Dict k v -> b
foldr fn nil (Dict t) =
  Tree.foldr fn nil t

{-| Returns a list of all the key-value pairs in the dictionary, in order
from lowest to highest by key. -}
toList: Dict k v -> List (k, v)
toList (Dict t) = Tree.toList t

{-| Returns a list of all the keys in the dictionary, in order from lowest
to highest. -}
keys: Dict k v -> List k
keys (Dict t) = Tree.keys t

{-| Returns a list of all the values in the dictionary, in order of their
corresponding keys, from lowest to highest. -}
values: Dict k v -> List v
values (Dict t) = Tree.values t

{-| Constructs a new dictionary from a list of key-value pairs. -}
fromList: Cmp k -> List (k, v) -> Dict k v
fromList cmp xs =
  Dict (Tree.fromList cmp xs)
