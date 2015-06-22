(** Sets of disequalities over terms. *)
(** it is guaranteed that for any pair (x,y) in the set, x<=y re [Sld_term.compare].*)

include Util.OrderedContainer with type elt = Sld_tpair.t

val parse : (Sld_tpair.t, 'a) MParser.parser
val subst : Sld_term.substitution -> t -> t

val terms : t -> Sld_term.Set.t
val vars : t -> Sld_term.Set.t

val to_string_list : t -> string list
val to_melt : t -> Latex.t

val unify_partial : ?inverse:bool -> t Sld_term.unifier
(** [unify_partial Option.some (Sld_term.empty_subst, ()) d d'] computes a 
    substitution [theta] such that [d[theta]] is a subset of [d']. 
    If the optional argument [~inverse:false] is set to [true] then a 
    substitution is computed such that [d] is a subset of [d'[theta]]. *)

val subsumed : Sld_uf.t -> t -> t -> bool
(** [subsumed eqs d d'] is true iff [d] can be rewritten using the equalities
    in [eqs] such that it becomes a subset of [d']. *)

val norm : Sld_uf.t -> t -> t
(** Rename all variables involved by their representative in the UF structure 
    and re-order pair members if necessary. *) 
