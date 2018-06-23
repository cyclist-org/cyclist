(** A stack structure for prophecy variables. *)

include Utilsigs.BasicType

val parse : (Sl_term.t * int, 'a) MParser.parser
val to_string_list : t -> string list

val empty : t
val is_empty : t -> bool

val find : Sl_term.t -> t -> int
val add : Sl_term.t -> int -> t -> t
val union : t -> t -> t

val fold : (Sl_term.t -> int -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (Sl_term.t -> int -> bool) -> t -> bool

val diff : t -> t -> t
(** [diff rho rho'] returns the structure given by removing all variables in
    [rho'] from [rho] *)

val bindings : t -> (Sl_term.t * int) list
(** Return mapping as a list of pairs of terms and values
	 Additional guarantees:
- Pairs are ordered lexicographically, based on [Sl_term.compare].
*)
val of_list : (Sl_term.t * int) list -> t

val subst : Sl_subst.t -> t -> t

val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val equates : t -> Sl_term.t -> int -> bool
(** Does a stack struct holds a term with a val? *)

val subsumed : t -> t -> bool
(** [subsumed rho rho'] is true iff uf' |- uf using the normal equality rules. *)

(*val unify_partial : ?inverse:bool -> t Sl_unifier.t *)
(** [unify_partial Option.some (Sl_subst.empty, ()) u u'] computes a
    substitution [theta] such that [u'] |- [u[theta]].
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)
