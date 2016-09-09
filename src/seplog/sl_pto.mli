(** Points-to atom, consisting of a pair of a term and a list of terms. *)

include Utilsigs.BasicType with type t = Sl_term.t * Sl_term.FList.t

val subst : Sl_subst.t -> t -> t
val to_melt : t -> Latex.t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t
val parse : (t, 'a) MParser.parser

val unify : t Sl_unifier.t
(** Compute substitution that unifies two points-tos. *)

val norm : Sl_uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil] 
    with a variable. *)
    
val record_type : t -> (Sl_term.t * int)