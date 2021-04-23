(** Sets of disequalities over terms. *)

(** it is guaranteed that for any pair (x,y) in the set, x<=y re [Term.compare].*)

include Lib.OrderedContainer with type elt = Tpair.t

val parse : (Tpair.t, 'a) MParser.parser

val subst : Subst.t -> t -> t

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val to_string_list : t -> string list

val unify_partial :
     ?inverse:bool
  -> ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** [unify_partial d d' Unification.trivial_continuation Unify.empty_state]
    computes a substitution [theta] such that [d[theta]] is a subset of [d'].
    If the optional argument [~inverse:false] is set to [true] then a
    substitution is computed such that [d] is a subset of [d'[theta]]. *)

val biunify_partial :
     ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val subsumed : Uf.t -> t -> t -> bool
(** [subsumed eqs d d'] is true iff [d] can be rewritten using the equalities
    in [eqs] such that it becomes a subset of [d']. *)

val norm : Uf.t -> t -> t
(** Rename all variables involved by their representative in the UF structure
    and re-order pair members if necessary. *)
