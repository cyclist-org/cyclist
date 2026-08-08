type ('a, 'b) continuation = 'a -> 'b option

val trivial_continuation : ('a, 'a) continuation

type 'a state_update = 'a * 'a
type ('a, 'b) realizer = ('a -> 'b) -> 'b

type ('a, 'b, 'c) cps_unifier =
  'c -> 'c -> ('a, 'b) continuation -> ('a, 'b) continuation

type ('a, 'b, 'c) cps_backtracker =
  'c -> 'c -> ('a, 'b) continuation -> 'a -> 'b list

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

val backtrack : ('a, 'b, 'c) cps_unifier -> ('a, 'b, 'c) cps_backtracker

val transform :
  ('d -> 'a) ->
  ('d -> 'a -> 'd) ->
  ('a, 'b, 'c) cps_unifier ->
  ('d, 'b, 'c) cps_unifier
