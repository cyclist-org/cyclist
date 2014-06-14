module type PRIOQ =
  sig
    type 'a left_heap

    val mk_empty : ('a -> 'a -> int) -> ('a left_heap)
    val put : 'a -> 'a left_heap -> 'a left_heap
    val take : 'a left_heap -> ('a * 'a left_heap)
    val hd : 'a left_heap -> 'a
    val tl : 'a left_heap -> 'a left_heap
    val is_empty : 'a left_heap -> bool
    val singleton : ('a -> 'a -> int) -> 'a -> ('a left_heap)
    val to_list : 'a left_heap -> 'a list
  end

module Prioq : PRIOQ =
  struct
    type 'a heap = E | T of int * 'a * 'a heap * 'a heap
    type 'a left_heap = ('a -> 'a -> int) * ('a heap)

    let rank = function E -> 0 | T (r,_,_,_) -> r

    let makeT x a b =
      if rank a >= rank b then T (rank b + 1, x, a, b)
      else T (rank a + 1, x, b, a)

    let rec merge cmp h1 h2 = match h1, h2 with
      | _, E -> h1
      | E, _ -> h2
      | T (_, x, a1, b1), T (_, y, a2, b2) ->
        if (cmp x y)<0 then makeT x a1 (merge cmp b1 h2)
        else makeT y a2 (merge cmp h1 b2)

    let insert cmp x h = merge cmp (T (1, x, E, E)) h

    let find_min = function
      | E -> invalid_arg "find_min"
      | T (_, x, _, _) -> x

    let delete_min cmp = function
      | E -> invalid_arg "delete_min"
      | T (_, x, a, b) -> merge cmp a b

    let mk_empty cmp = (cmp, E)
    let put el (cmp, h) = (cmp, insert cmp el h)
    let hd (_, h) = find_min h
    let tl (cmp, h) = (cmp, delete_min cmp h)
    let take lh = (hd lh, tl lh)
    let is_empty (_, h) = match h with
      | E -> true
      | _ -> false
    let singleton cmp el = put el (mk_empty cmp)

    let rec to_list pq =
      if is_empty pq then [] else
      let hd, tl = take pq in
      hd :: (to_list tl)
  end
