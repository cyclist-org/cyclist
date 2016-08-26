(** A union-find structure for ASL terms. *)

include Util.BasicType

val parse : (Asl_tpair.t, 'a) MParser.parser
val to_melt : t -> Latex.t
val to_string_list : t -> string list

val empty : t
val is_empty : t -> bool

val find : Asl_term.t -> t -> Asl_term.t
val add : Asl_tpair.t -> t -> t
val union : t -> t -> t

val fold : (Asl_term.t -> Asl_term.t -> 'a -> 'a) -> t -> 'a -> 'a 
val for_all : (Asl_term.t -> Asl_term.t -> bool) -> t -> bool

val all_members_of : t -> t -> bool
(** [all_members_of eqs eqs'] returns true iff all equalities in [eqs] are also
    in [eqs'] *)
    
val diff : t -> t -> t
(** [diff eqs eqs'] returns the structure given by removing all equalities in
    [eqs'] from [eqs] *)

val bindings : t -> Asl_tpair.t list
(** Return mapping as a list of pairs, where pair members are ordered by *)
(** [Asl_term.compare].  Additional guarantees:
- Each term appears at most once on the LHS of any pair.
- Pairs are ordered lexicographically, based on [Asl_term.compare]. 
*)
val of_list : Asl_tpair.t list -> t

val subst : Asl_subst.t -> t -> t

val terms : t -> Asl_term.Set.t
val vars : t -> Asl_term.Set.t

val to_fopl : t -> Fopl.formula

val simple_equates : t -> Asl_term.t -> Asl_term.t -> bool

val equates : t -> Asl_term.t -> Asl_term.t -> bool
(** Does a UF struct make two terms equal? *)

val subsumed : t -> t -> bool
(** [subsumed uf uf'] is true iff uf' |- uf using the normal equality rules. *)

val unify_partial : ?inverse:bool -> t Asl_unifier.t
(** [unify_partial Option.some (Sl_subst.empty, ()) u u'] computes a 
    substitution [theta] such that [u'] |- [u[theta]]. 
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)

val remove : Asl_term.t -> t -> t
