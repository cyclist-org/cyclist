(** A union-find structure for SL terms. *)

type t

val parse : (Sl_tpair.t, 'a) MParser.parser
val to_melt : t -> Latex.t
val to_string_list : t -> string list
val pp : Format.formatter -> t -> unit

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

val subst_subsumed : 
  t -> Sl_term.unifier_state -> Sl_term.unifier_state option
(** Compute whether a substitution could be obtained by rewriting under 
    equalities in first argument.  Meant to be used with unifiers to produce
    subsumption routines. *)

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val equates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val unify_partial : ?inverse:bool -> t Sl_term.unifier
(** [unify_partial Option.some (Sl_term.empty_subst, ()) u u'] computes a 
    substitution [theta] such that [u'] |- [u[theta]]. 
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)

val remove : Sl_term.t -> t -> t
(** FIXME why is this here? *)
