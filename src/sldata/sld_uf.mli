(** A union-find structure for SL terms. *)

type t

val parse : (Sld_tpair.t, 'a) MParser.parser
val to_melt : t -> Latex.t
val to_string_list : t -> string list
val pp : Format.formatter -> t -> unit

val empty : t
val is_empty : t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int

val find : Sld_term.t -> t -> Sld_term.t
val add : Sld_tpair.t -> t -> t
val union : t -> t -> t

val fold : (Sld_term.t -> Sld_term.t -> 'a -> 'a) -> t -> 'a -> 'a 

val all_members_of : t -> t -> bool
(** [all_members_of eqs eqs'] returns true iff all equalities in [eqs] are also
    in [eqs'] *)
    
val diff : t -> t -> t
(** [diff eqs eqs'] returns the structure given by removing all equalities in
    [eqs'] from [eqs] *)

val bindings : t -> Sld_tpair.t list
(** Return mapping as a list of pairs, where pair members are ordered by *)
(** [Sld_term.compare].  Additional guarantees:
- Each term appears at most once on the LHS of any pair.
- Pairs are ordered lexicographically, based on [Sld_term.compare]. 
*)
val of_list : Sld_tpair.t list -> t

val subst : Sld_term.substitution -> t -> t

val subst_subsumed : 
  t -> Sld_term.unifier_state -> Sld_term.unifier_state option
(** Compute whether a substitution could be obtained by rewriting under 
    equalities in first argument.  Meant to be used with unifiers to produce
    subsumption routines. *)

val terms : t -> Sld_term.Set.t
val vars : t -> Sld_term.Set.t

val equates : t -> Sld_term.t -> Sld_term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val unify_partial : ?inverse:bool -> t Sld_term.unifier
(** [unify_partial Option.some (Sld_term.empty_subst, ()) u u'] computes a 
    substitution [theta] such that [u'] |- [u[theta]]. 
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)

val remove : Sld_term.t -> t -> t
(** FIXME why is this here? *)
