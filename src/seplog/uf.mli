(** A union-find structure for SL terms. *)

include Lib.BasicType

val parse : (Tpair.t, 'a) MParser.parser

val to_string_list : t -> string list

val empty : t

val is_empty : t -> bool

val find : Term.t -> t -> Term.t

val add : Tpair.t -> t -> t

val union : t -> t -> t

val fold : (Term.t -> Term.t -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (Term.t -> Term.t -> bool) -> t -> bool

val diff : t -> t -> t
(** [diff eqs eqs'] returns the structure given by removing all equalities in
    [eqs'] from [eqs] *)

val bindings : t -> Tpair.t list
(** Return mapping as a list of pairs, where pair members are ordered by  
 [Term.compare].  Additional guarantees:
- Each term appears at most once on the LHS of any pair.
- Pairs are ordered lexicographically, based on [Term.compare].
*)

val of_list : Tpair.t list -> t

val subst : Subst.t -> t -> t

val subst_subsumed : t -> Unify.Unidirectional.continuation
(** Compute whether a substitution could be obtained by rewriting under
    equalities in first argument.  Meant to be used with unifiers to produce
    subsumption routines. *)

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val equates : t -> Term.t -> Term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val unify_partial :
     ?inverse:bool
  -> ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** [unify_partial Option.some (Term.empty_subst, ()) u u'] computes a
    substitution [theta] such that [u'] |- [u[theta]].
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)

val biunify_partial :
     ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val remove : Term.t -> t -> t
(** FIXME why is this here? *)
