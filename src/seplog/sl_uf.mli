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

val to_subst : t -> Sl_term.substitution
(** Turn a UF structure into a substitution map. *)
(** For example, 
- [x'=y'] gives [y'/x']
- [x'=nil] gives [nil/x']
- [x'=y] gives [y/x']
- [nil=y] gives [nil/y] (NB this was incorrectly ignored previously) 
- [x=y] gives [y/x]
*)

val subst : Sl_term.substitution -> t -> t
val vars : t -> Sl_term.Set.t

val equates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val part_unify : t Sl_term.gen_unifier

val remove : Sl_term.t -> t -> t
(** FIXME why is this here? *)
