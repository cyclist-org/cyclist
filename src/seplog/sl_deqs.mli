(** Sets of disequalities over terms. *)
(** it is guaranteed that for any pair (x,y) in the set, x<=y re [Sl_term.compare].*)

include Util.OrderedContainer with type elt = Sl_tpair.t

val parse : (Sl_tpair.t, 'a) MParser.parser
val subst : Sl_term.substitution -> t -> t
val vars : t -> Sl_term.Set.t

val to_string_list : t -> string list
val to_melt : t -> Latex.t

val part_unify : t Sl_term.gen_unifier

val subsumed : Sl_uf.t -> t -> t -> bool
