(** Multiset of arrays. *)

include Util.OrderedContainer with type elt = Asl_array.t

val subst : Asl_subst.t -> t -> t
val terms : t -> Asl_term.Set.t
val vars : t -> Asl_term.Set.t
val to_string_list : t -> string list
val to_melt : t -> Latex.t

val subsumed : ?total:bool -> Asl_uf.t -> t -> t -> bool
(** [subsumed eqs arrs arrs'] is true iff [arrs] can be rewritten using the 
    equalities [arrs] such that it becomes equal to [arrs']. 
    If the optional argument [~total=true] is set to [false] then 
    check if rewriting could make the first multiset a sub(multi)set of 
    the second. *)
    
val unify : ?total:bool -> t Asl_unifier.t
(** Compute substitution that would make the two multisets equal. 
    If the optional argument [~total=true] is set to [false] then 
    compute a substitution that would make the first multiset a sub(multi)set of 
    the second. *)

val norm : Asl_uf.t -> t -> t
(** Replace all terms with their UF representative. *)
    