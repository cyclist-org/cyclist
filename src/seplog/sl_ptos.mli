(** Multiset of points-tos. *)

include Lib.OrderedContainer with type elt = Sl_pto.t

val subst : Sl_subst.t -> t -> t

val terms : t -> Sl_term.Set.t

val vars : t -> Sl_term.Set.t

val to_string_list : t -> string list

val subsumed : ?total:bool -> Sl_uf.t -> t -> t -> bool
(** [subsumed eqs ptos ptos'] is true iff [ptos] can be rewritten using the
    equalities [eqs] such that it becomes equal to [ptos'].
    If the optional argument [~total=true] is set to [false] then
    check if rewriting could make the first multiset a sub(multi)set of
    the second. *)

val unify :
     ?total:bool
  -> ?update_check:Sl_unify.Unidirectional.update_check
  -> t Sl_unify.Unidirectional.unifier
(** Compute substitution that would make the two multisets equal.
    If the optional argument [~total=true] is set to [false] then
    compute a substitution that would make the first multiset a sub(multi)set of
    the second. *)

val biunify :
     ?total:bool
  -> ?update_check:Sl_unify.Bidirectional.update_check
  -> t Sl_unify.Bidirectional.unifier

val norm : Sl_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil]
    with a variable. *)
