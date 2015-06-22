(** Points-to atom, consisting of a pair of a term and a list of terms. *)

include Util.BasicType with type t = Sld_term.t * Sld_term.FList.t

val subst : Sld_term.substitution -> t -> t
val to_melt : t -> Latex.t
val terms : t -> Sld_term.Set.t
val vars : t -> Sld_term.Set.t

val unify : t Sld_term.unifier
(** Compute substitution that unifies two points-tos. *)

val norm : Sld_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
    
val record_type : t -> (Sld_term.t * int)
