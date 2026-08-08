(** A library of types and combinators supporting continuation-passing-style
    unifiers *)

type ('a, 'b) continuation = 'a -> 'b option
(** The type of continuations accepted by CPS-unifiers *)

val trivial_continuation : ('a, 'a) continuation

type 'a state_update = 'a * 'a
type ('a, 'b) realizer = ('a -> 'b) -> 'b

type ('a, 'b, 'c) cps_unifier =
  'c -> 'c -> ('a, 'b) continuation -> ('a, 'b) continuation
(** The type of continuation-passing-style unifiers that may accept an initial
    solution state. [('a, 'b, 'c) cps_unifier] is the type of a CPS-unifier that
    unifies terms of type 'c producing a solution of type 'a by extending a
    given initial state, which is then passed to a continuation that returns a
    value of type 'b option. If unification is impossible, then the unifier
    should return None immediately without calling the continuation. *)

type ('a, 'b, 'c) cps_backtracker =
  'c -> 'c -> ('a, 'b) continuation -> 'a -> 'b list

(** This functor makes a mk_unifier function for a particular implementation of
    a container. *)
module MakeUnifier (T : sig
  type t
  type elt

  val empty : t
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val add : elt -> t -> t
  val choose : t -> elt
  val remove : elt -> t -> t
  val find_map : (elt -> 'a option) -> t -> 'a option
end) : sig
  val mk_unifier :
    bool ->
    bool ->
    (T.elt -> T.elt -> ('a -> 'b option) -> 'a -> 'b option) ->
    T.t ->
    T.t ->
    ('a -> 'b option) ->
    'a ->
    'b option
end

(** cps-style unifier combinators *)

val backtrack : ('a, 'b, 'c) cps_unifier -> ('a, 'b, 'c) cps_backtracker
(** [backtrack u] takes a cps-style unifier [u] and produces a backtracking
    unifier that returns a list of all possible solutions returned by [u] such
    that [cont solution] is not None. *)

val transform :
  ('d -> 'a) ->
  ('d -> 'a -> 'd) ->
  ('a, 'b, 'c) cps_unifier ->
  ('d, 'b, 'c) cps_unifier
