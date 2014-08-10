(** A union-find structure for SL terms. *)

type t

val parse : (Sl_tpair.t, 'a) MParser.parser
val to_melt : t -> Latex.t
val to_string_list : t -> string list

val empty : t
val is_empty : t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int

val find : Sl_term.t -> t -> Sl_term.t
val add : Sl_tpair.t -> t -> t
val union : t -> t -> t

val bindings : t -> Sl_tpair.t list
(** Return mapping as a list of pairs, where pair members are ordered by *)
(** [Sl_term.compare].  Additional guarantees:
- Each term appears at most once on the LHS of any pair.
- Pairs are ordered lexicographically, based on [Sl_term.compare]. 
*)
val of_list : Sl_tpair.t list -> t

val subst : Sl_term.substitution -> t -> t

val subst_subsumed : t -> Sl_term.substitution -> Sl_term.substitution option
(** Compute whether a substitution could be obtained by rewriting under 
    equalities in first argument.  Meant to be used with unifiers to produce
    subsumption routines. *)

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val equates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val unify_with_part : t Sl_term.gen_unifier

val remove : Sl_term.t -> t -> t
(** FIXME why is this here? *)
