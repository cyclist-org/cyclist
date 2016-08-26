(** Array, consisting of a pair of a terms. *)

include Util.BasicType with type t = Asl_term.t * Asl_term.t

val subst : Asl_subst.t -> t -> t
val to_melt : t -> Latex.t
val terms : t -> Asl_term.Set.t
val vars : t -> Asl_term.Set.t
val parse : (t, 'a) MParser.parser

val unify : t Asl_unifier.t
(** Compute substitution that unifies two arrays. *)

val norm : Asl_uf.t -> t -> t
(** Replace all terms with their UF representative. *)