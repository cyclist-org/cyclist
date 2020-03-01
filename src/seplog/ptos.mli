(** Multiset of points-tos. *)

include Lib.OrderedContainer with type elt = Pto.t

val subst : Subst.t -> t -> t

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val to_string_list : t -> string list

val subsumed : ?total:bool -> Uf.t -> t -> t -> bool
(** [subsumed eqs ptos ptos'] is true iff [ptos] can be rewritten using the
    equalities [eqs] such that it becomes equal to [ptos'].
    If the optional argument [~total=true] is set to [false] then
    check if rewriting could make the first multiset a sub(multi)set of
    the second. *)

val unify :
     ?total:bool
  -> ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** Compute substitution that would make the two multisets equal.
    If the optional argument [~total=true] is set to [false] then
    compute a substitution that would make the first multiset a sub(multi)set of
    the second. *)

val biunify :
     ?total:bool
  -> ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val norm : Uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil]
    with a variable. *)
