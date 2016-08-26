(** Sets of disequalities over terms (of the form x <= y). *)

include Util.OrderedContainer with type elt = Asl_tpair.t

val parse : (Asl_tpair.t, 'a) MParser.parser
val subst : Asl_subst.t -> t -> t

val terms : t -> Asl_term.Set.t
val vars : t -> Asl_term.Set.t

val to_fopl : t -> Fopl.formula
(** Convert disequality to First Order Predicate Logic counterpart. *)

val to_string_list : t -> string list
val to_melt : t -> Latex.t

val unify_partial : ?inverse:bool -> t Asl_unifier.t
(** [unify_partial Option.some (Sl_subst.empty, ()) d d'] computes a 
    substitution [theta] such that [d[theta]] is a subset of [d']. 
    If the optional argument [~inverse:false] is set to [true] then a 
    substitution is computed such that [d] is a subset of [d'[theta]]. *)

val subsumed : Asl_uf.t -> t -> t -> bool
(** [subsumed eqs d d'] is true iff [d] can be rewritten using the equalities
    in [eqs] such that it becomes a subset of [d']. *)

val norm : Asl_uf.t -> t -> t
(** Rename all variables involved by their representative in the UF structure 
    and re-order pair members if necessary. *) 