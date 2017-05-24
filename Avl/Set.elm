module Avl.Set exposing
  ( size
  , empty
  , isEmpty
  , singleton
  , member
  , insert
  , remove
  , foldl
  , foldr
  , toList
  , union
  , intersect
  , difference
  , subset
  , fromList
  , Set )

{-| A set of elements. The elements can have any type,
since the user is required to provide an explicit comparison
function with type `k -> k -> Order`.

# Sets
@docs Set

# Build
@docs empty, singleton, insert, remove, union, intersect, difference

# Query
@docs isEmpty, member, size, subset

# Lists
@docs toList, fromList

# Transform
@docs foldl, foldr

-}

import Avl exposing (Cmp)
import Avl.Tree as Tree

{-| A set of elements. -}
type Set k = Set (Tree.Node k Bool)

{-| The size (or cardinality) of the set. -}
size: Set k -> Int
size (Set t) = Tree.size t

{-| The empty set. -}
empty: Set k
empty = Set (Tree.empty)

{-| `True` if the set is empty, `False` otherwise. -}
isEmpty: Set k -> Bool
isEmpty set = (size set) == 0

{-| Constructs a set with a single element. -}
singleton: k -> Set k
singleton key =
  Set (Tree.singleton key True)

{-| `True` if the given element is in the set, `False` otherwise. -}
member: Cmp k -> k -> Set k -> Bool
member cmp key (Set t) = Tree.member cmp key t

{-| Inserts an element into the set. -}
insert: Cmp k -> k -> Set k -> Set k
insert cmp key (Set t) =
  Set (Tree.insert cmp key True t)

{-| Removes an element from the set. -}
remove: Cmp k -> k -> Set k -> Set k
remove cmp key (Set t) =
  Set (Tree.remove cmp key t)

{-| Folds the given function over the elements of the set, in order from
the least element to the greatest. -}
foldl: (k -> b -> b) -> b -> Set k -> b
foldl fn nil (Set t) =
  Tree.foldl (\k _ b -> fn k b) nil t

{-| Folds the given function over the elements of the set, in order from
the greatest element to the least. -}
foldr: (k -> b -> b) -> b -> Set k -> b
foldr fn nil (Set t) =
  Tree.foldr (\k _ b -> fn k b) nil t

{-| Returns a list containing all of the set's elements, in order from
least to greatest. -}
toList: Set k -> List k
toList (Set t) = Tree.keys t

{-| Constructs the union of the given sets. -}
union: Cmp k -> Set k -> Set k -> Set k
union cmp (Set t1) (Set t2) =
  Set (Tree.union cmp t1 t2)

{-| Constructs the intersection of the given sets. -}
intersect: Cmp k -> Set k -> Set k -> Set k
intersect cmp (Set t1) (Set t2) =
  Set (Tree.intersect cmp t1 t2)

{-| Constructs the set difference of the given sets. -}
difference: Cmp k -> Set k -> Set k -> Set k
difference cmp (Set t1) (Set t2) =
  Set (Tree.difference cmp t1 t2)

{-| `True` if the first set is a subset of the second,
`False` otherwise. -}
subset: Cmp k -> Set k -> Set k -> Bool
subset cmp s1 s2 =
  isEmpty (difference cmp s1 s2)

{-| Constructs a set from a list of elements. -}
fromList: Cmp k -> List k -> Set k
fromList cmp xs =
  List.foldl (\k nil -> insert cmp k nil) empty xs
