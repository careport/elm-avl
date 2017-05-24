module Avl exposing (Cmp)

{-| AVL tree-based collections.

Unlike the elm-core `Dict` and `Set` types, the collections
defined in this package can be used with any type of key,
since they require the user to provide an explicit comparison
function.

The obvious downside of this approach is that the user must be
vigilant to always use the *same* comparison function on the same
data structure.

The upside, of course, is that you are not limited to using
`comparable` types as keys.

(The implementation is based on Blelloch, Ferizovic, and Sun:
"[Just Join for Parallel Ordered Sets](https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf)," SPAA 2016.)

# Comparison
@docs Cmp

-}

{-| The type of comparison functions over arbitrary key types. -}
type alias Cmp k = k -> k -> Order
