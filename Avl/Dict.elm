module Avl.Dict exposing
  ( size
  , empty
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
  , Dict )

import Avl.Tree as Tree exposing (Cmp)

type Dict k v = Dict (Tree.Node k v)

size: Dict k v -> Int
size (Dict t) = Tree.size t

empty: Dict k v
empty = Dict Tree.empty

member: Cmp k -> k -> Dict k v -> Bool
member cmp key (Dict t) =
  Tree.member cmp key t

get: Cmp k -> k -> Dict k v -> Maybe v
get cmp key (Dict t) =
  Tree.get cmp key t

insert: Cmp k -> k -> v -> Dict k v -> Dict k v
insert cmp key val (Dict t) =
  Dict (Tree.insert cmp key val t)

remove: Cmp k -> k -> Dict k v -> Dict k v
remove cmp key (Dict t) =
  Dict (Tree.remove cmp key t)

update: Cmp k -> k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update cmp key fn (Dict t) =
  Dict (Tree.update cmp key fn t)

foldl: (k -> v -> b -> b) -> b -> Dict k v -> b
foldl fn nil (Dict t) =
  Tree.foldl fn nil t

foldr: (k -> v -> b -> b) -> b -> Dict k v -> b
foldr fn nil (Dict t) =
  Tree.foldr fn nil t

toList: Dict k v -> List (k, v)
toList (Dict t) = Tree.toList t

keys: Dict k v -> List k
keys (Dict t) = Tree.keys t

values: Dict k v -> List v
values (Dict t) = Tree.values t
