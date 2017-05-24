module Avl.Tree exposing (..)

import Avl exposing (Cmp)

type Node k v
  = Br Int (Node k v) (k, v) (Node k v)
  | Lf

empty: Node k v
empty = Lf

singleton: k -> v -> Node k v
singleton key val =
  br Lf (key, val) Lf

size: Node k v -> Int
size t =
  case t of
    Lf ->
      0

    Br _ l _ r ->
      1 + (size l) + (size r)

height: Node k v -> Int
height t =
  case t of
    Lf ->
      0

    Br h _ _ _ ->
      h

get: Cmp k -> k -> Node k v -> Maybe v
get cmp key t =
  case t of
    Lf ->
      Nothing

    Br _ l (k, v) r ->
      case cmp key k of
        EQ -> Just v
        LT -> get cmp key l
        GT -> get cmp key r

member: Cmp k -> k -> Node k v -> Bool
member cmp k t =
  case get cmp k t of
    Nothing -> False
    _       -> True

insert: Cmp k -> k -> v -> Node k v -> Node k v
insert cmp key val t =
  let
    (l, _, r) = split cmp key t
  in
    join l (key, val) r

remove: Cmp k -> k -> Node k v -> Node k v
remove cmp key t =
  let
    (l, _, r) = split cmp key t
  in
    join2 l r

update: Cmp k -> k -> (Maybe v -> Maybe v) -> Node k v -> Node k v
update cmp key fn t =
  let
    (l, m, r) = split cmp key t
    mv = Maybe.map Tuple.second m
  in
    case fn mv of
      Nothing  -> join2 l r
      Just val -> join l (key, val) r

union: Cmp k -> Node k v -> Node k v -> Node k v
union cmp t1 t2 =
  case (t1, t2) of
    (Lf, _) -> t2
    (_, Lf) -> t1

    (_, Br _ l2 ((k2, _) as kv2) r2) ->
      let
        (l1, _, r1) = split cmp k2 t1
        tl = union cmp l1 l2
        tr = union cmp r1 r2
      in
        join tl kv2 tr

intersect: Cmp k -> Node k v -> Node k v -> Node k v
intersect cmp t1 t2 =
  case (t1, t2) of
    (Lf, _) -> Lf
    (_, Lf) -> Lf

    (_, Br _ l2 ((k2, _) as kv2) r2) ->
      let
        (l1, b, r1) = split cmp k2 t1
        tl = intersect cmp l1 l2
        tr = intersect cmp r1 r2
      in
        case b of
          Nothing ->
            join2 tl tr

          Just _ ->
            join tl kv2 tr

difference: Cmp k -> Node k v -> Node k v -> Node k v
difference cmp t1 t2 =
  case (t1, t2) of
    (Lf, _) -> Lf
    (_, Lf) -> t1

    (_, Br _ l2 (k2, _) r2) ->
      let
        (l1, _, r1) = split cmp k2 t1
        tl = difference cmp l1 l2
        tr = difference cmp r1 r2
      in
        join2 tl tr

foldl: (k -> v -> b -> b) -> b -> Node k v -> b
foldl f nil t =
  case t of
    Lf ->
      nil

    Br _ lt (k, v) rt ->
      let
        lres = foldl f nil lt
        nres = f k v lres
      in
        foldl f nres rt

foldr: (k -> v -> b -> b) -> b -> Node k v -> b
foldr f nil t =
  case t of
    Lf ->
      nil

    Br _ lt (k, v) rt ->
      let
        rres = foldr f nil rt
        nres = f k v rres
      in
        foldr f nres lt

foldk: (k -> v -> b -> (b -> b) -> b) -> b -> Node k v -> (b -> b) -> b
foldk fn nil t kont =
  case t of
    Lf ->
      kont nil

    Br _ lt (k, v) rt ->
      fn k v nil (\nil1 -> foldk fn nil1 lt (\nil2 -> foldk fn nil2 rt kont))

toList: Node k v -> List (k, v)
toList t =
  foldr (\k v xs -> (k, v) :: xs) [] t

keys: Node k v -> List k
keys t =
  foldr (\k _ xs -> k::xs) [] t

values: Node k v -> List v
values t =
  foldr (\_ v xs -> v::xs) [] t

fromList: Cmp k -> List (k, v) -> Node k v
fromList cmp xs =
  List.foldl (\(k, v) nil -> insert cmp k v nil) empty xs

br: Node k v -> (k, v) -> Node k v -> Node k v
br left kv right =
  let
    h = 1 + (max (height left) (height right))
  in
    Br h left kv right

split: Cmp k -> k -> Node k v -> (Node k v, Maybe (k, v), Node k v)
split cmp key t =
  case t of
    Lf ->
      (Lf, Nothing, Lf)

    Br _ lt (k, v) rt ->
      case cmp key k of
        EQ ->
          (lt, Just (k, v), rt)

        LT ->
          let
            (ll, b, lr) = split cmp key lt
          in
            (ll, b, join lr (k, v) rt)

        GT ->
          let
            (rl, b, rr) = split cmp key rt
          in
            (join lt (k, v) rl, b, rr)

join: Node k v -> (k, v) -> Node k v -> Node k v
join lt kv rt =
  if height lt > (height rt) + 1 then
    joinRight lt kv rt
  else if height rt > (height lt) + 1 then
    joinLeft lt kv rt
  else
    br lt kv rt

joinRight: Node k v -> (k, v) -> Node k v -> Node k v
joinRight lt kv rt =
  case lt of
    Br _ l kvp c ->
      if height c <= (height rt) + 1 then
        let
          tp = br c kv rt
        in
          if height tp <= (height l) + 1 then
            br l kvp tp
          else
            rotateLeft (br l kvp (rotateRight tp))
      else
        let
          tp = joinRight c kv rt
          tpp = br l kvp tp
        in
          if height tp <= (height l) + 1 then
            tpp
          else
            rotateLeft tpp
    _ ->
      Debug.crash "impossible: encountered leaf in joinRight"

joinLeft: Node k v -> (k, v) -> Node k v -> Node k v
joinLeft lt kv rt =
  case rt of
    Br _ c kvp r ->
      if height c <= (height lt) + 1 then
        let
          tp = br lt kv c
        in
          if height tp <= (height r) + 1 then
            br tp kvp r
          else
            rotateRight (br (rotateLeft tp) kvp r)
      else
        let
          tp = joinLeft lt kv c
          tpp = br tp kvp r
        in
          if height tp <= (height r) + 1 then
            tpp
          else
            rotateRight tpp
    _ ->
      Debug.crash "impossible: encountered leaf in joinLeft"

rotateLeft: Node k v -> Node k v
rotateLeft t =
  case t of
    Br _ (Br _ llt vl lrt) v rt ->
      br llt vl (br lrt v rt)

    _ ->
      Debug.crash "AVL invariant violated in rotateLeft"

rotateRight: Node k v -> Node k v
rotateRight t =
  case t of
    Br _ lt v (Br _ rlt vr rrt) ->
      br (br lt v rlt) vr rrt

    _ ->
      Debug.crash "AVL invariant violated in rotateRight"

-- splitLast is an internal function that can only be used on
-- non-empty trees
splitLast: Node k v -> (Node k v, (k, v))
splitLast t =
  case t of
    Lf ->
      Debug.crash "splitLast called on empty tree"

    Br _ l kv Lf ->
      (l, kv)

    Br _ l kv r ->
      let
        (tp, kvp) = splitLast r
      in
        (join l kv tp, kvp)

join2: Node k v -> Node k v -> Node k v
join2 l r =
  case l of
    Lf ->
      r

    _ ->
      let
        (lp, kv) = splitLast l
      in
        join lp kv r
