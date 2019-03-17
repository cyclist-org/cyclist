(** A union-find structure for SL terms. *)

include Utilsigs.BasicType

val parse : (Sl_tpair.t, 'a) MParser.parser

val to_string_list : t -> string list

val empty : t

val is_empty : t -> bool

val find : Sl_term.t -> t -> Sl_term.t

val add : Sl_tpair.t -> t -> t

val union : t -> t -> t

val fold : (Sl_term.t -> Sl_term.t -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (Sl_term.t -> Sl_term.t -> bool) -> t -> bool

val diff : t -> t -> t
(** [diff eqs eqs'] returns the structure given by removing all equalities in
    [eqs'] from [eqs] *)

val bindings : t -> Sl_tpair.t list
(** Return mapping as a list of pairs, where pair members are ordered by  
 [Sl_term.compare].  Additional guarantees:
- Each term appears at most once on the LHS of any pair.
- Pairs are ordered lexicographically, based on [Sl_term.compare].
*)

val of_list : Sl_tpair.t list -> t

val subst : Sl_subst.t -> t -> t

val subst_subsumed : t -> Sl_unify.Unidirectional.continuation
(** Compute whether a substitution could be obtained by rewriting under
    equalities in first argument.  Meant to be used with unifiers to produce
    subsumption routines. *)

val terms : t -> Sl_term.Set.t

val vars : t -> Sl_term.Set.t

val equates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val unify_partial :
     ?inverse:bool
  -> ?update_check:Sl_unify.Unidirectional.update_check
  -> t Sl_unify.Unidirectional.unifier
(** [unify_partial Option.some (Sl_term.empty_subst, ()) u u'] computes a
    substitution [theta] such that [u'] |- [u[theta]].
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)

val biunify_partial :
     ?update_check:Sl_unify.Bidirectional.update_check
  -> t Sl_unify.Bidirectional.unifier

val remove : Sl_term.t -> t -> t
(** FIXME why is this here? *)
