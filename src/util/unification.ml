open Lib

(** A library of types and combinators supporting 
    continuation-passing-style unifiers *)

(** The type of continuations accepted by CPS-unifiers *)
type ('a, 'b) continuation = 'a -> 'b option

let trivial_continuation : ('a, 'a) continuation = Option.some

type 'a state_update = 'a * 'a

type ('a, 'b) realizer = ('a -> 'b) -> 'b

(** The type of continuation-passing-style unifiers that may
    accept an initial solution state.
      [('a, 'b, 'c) cps_unifier] is the type of a CPS-unifier
    that unifies terms of type 'c producing a solution of type 'a
    by extending a given initial state, which is then passed to a
    continuation that returns a value of type 'b option.
    If unification is impossible, then the unifier should return
    None immediately without calling the continuation. *)
type ('a, 'b, 'c) cps_unifier =
  'c -> 'c -> ('a, 'b) continuation -> ('a, 'b) continuation

type ('a, 'b, 'c) cps_backtracker =
  'c -> 'c -> ('a, 'b) continuation -> 'a -> 'b list


(** This functor makes a mk_unifier function for a particular implementation of
    a container. *)
module MakeUnifier(
  T : sig
    type t
    type elt
    val empty : t
    val is_empty : t -> bool
    val equal : t -> t -> bool
    val add : elt -> t -> t
    val choose : t -> elt
    val remove : elt -> t -> t
    val find_map : (elt -> 'a option) -> t -> 'a option
  end) =
struct
  let mk_unifier total linear u =
    let rec unify marked_ys xs ys cont state =
      if T.is_empty xs then
        if (not total) ||
           (linear && T.is_empty ys) ||
           ((not linear) && (T.equal marked_ys ys))
        then cont state else None
      else
        let x = T.choose xs in
        let xs = T.remove x xs in
        let f y =
          let ys = if linear then T.remove y ys else ys in
          let marked_ys = if not linear then T.add y marked_ys else marked_ys in
          u x y
          (unify marked_ys xs ys cont)
          state in
        T.find_map f ys in
    unify T.empty
end


(** cps-style unifier combinators *)


(** [backtrack u] takes a cps-style unifier [u] and produces a
      backtracking unifier that returns a list of all possible
      solutions returned by [u] such that [cont solution] is not
      None. *)
let backtrack (u: ('a, 'b, 'c) cps_unifier) : ('a, 'b, 'c) cps_backtracker =
  fun x y cont init_state ->
    let res = ref [] in
    let valid state' =
      let v = cont state' in
      match v with
      | None -> None
      | Some state'' -> res := state'' :: !res ; None in
    let _ = u x y valid init_state in
    !res


(**  *)
let transform (extract: 'd -> 'a) (recombine: 'd -> 'a -> 'd)
    (u: ('a, 'b, 'c) cps_unifier) : ('d, 'b, 'c) cps_unifier =
  fun x y cont init_state ->
    let cont' v = cont (recombine init_state v) in
    u x y cont' (extract init_state)
