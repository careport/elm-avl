module Avl.Set exposing
  ( size
  , empty
  , member
  , insert
  , remove
  , foldl
  , foldr
  , toList
  , union
  , intersect
  , difference
  , Set )

import Avl.Tree as Tree exposing (Cmp)

type Set k = Set (Tree.Node k Bool)

size: Set k -> Int
size (Set t) = Tree.size t

empty: Set k
empty = Set (Tree.empty)

member: Cmp k -> k -> Set k -> Bool
member cmp key (Set t) = Tree.member cmp key t

insert: Cmp k -> k -> Set k -> Set k
insert cmp key (Set t) =
  Set (Tree.insert cmp key True t)

remove: Cmp k -> k -> Set k -> Set k
remove cmp key (Set t) =
  Set (Tree.remove cmp key t)

foldl: (k -> b -> b) -> b -> Set k -> b
foldl fn nil (Set t) =
  Tree.foldl (\k _ b -> fn k b) nil t

foldr: (k -> b -> b) -> b -> Set k -> b
foldr fn nil (Set t) =
  Tree.foldr (\k _ b -> fn k b) nil t

toList: Set k -> List k
toList (Set t) = Tree.keys t

union: Cmp k -> Set k -> Set k -> Set k
union cmp (Set t1) (Set t2) =
  Set (Tree.union cmp t1 t2)

intersect: Cmp k -> Set k -> Set k -> Set k
intersect cmp (Set t1) (Set t2) =
  Set (Tree.intersect cmp t1 t2)

difference: Cmp k -> Set k -> Set k -> Set k
difference cmp (Set t1) (Set t2) =
  Set (Tree.difference cmp t1 t2)
