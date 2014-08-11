(** Multiset of points-tos. *)

include Util.OrderedContainer with type elt = Sl_pto.t

val parse : (Sl_pto.t, 'a) MParser.parser
val subst : Sl_term.substitution -> t -> t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val to_string_list : t -> string list
val to_melt : t -> Latex.t

val subsumed : Sl_uf.t -> t -> t -> bool
(** [subsumed eqs ptos ptos'] is true iff [ptos] can be rewritten using the 
    equalities [eqs] such that it becomes equal to [ptos']. *)
    
val unify : (t, 'a) Sl_term.unifier
(** Compute substitution that would make the two multisets equal. *)

val unify_within : (t, 'a) Sl_term.unifier
(** Compute substitution that would make the first multiset a sub(multi)set of the
    second. *)
