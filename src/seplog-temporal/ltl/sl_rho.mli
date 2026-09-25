(** A stack structure for prophecy variables. *)
open Lib
open Seplog

include BasicType

val parse : (Term.t * int, 'a) MParser.parser
(* val to_melt : t -> Latex.t *)
val to_string_list : t -> string list

val empty : t
val is_empty : t -> bool

val find : Term.t -> t -> int
val add : Term.t -> int -> t -> t
val union : t -> t -> t

val fold : (Term.t -> int -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (Term.t -> int -> bool) -> t -> bool

val all_members_of : t -> t -> bool
(** [all_members_of eqs eqs'] returns true iff all variables in [rho] are also
    in [rho'] *)

val diff : t -> t -> t
(** [diff rho rho'] returns the structure given by removing all variables in
    [rho'] from [rho] *)

val bindings : t -> (Term.t * int) list
(** Return mapping as a list of pairs of terms and values
	 Additional guarantees:
- Pairs are ordered lexicographically, based on [Term.compare].
*)
val of_list : (Term.t * int) list -> t

val subst : Term.Subst.t -> t -> t

val terms : t -> Term.Set.t
val vars : t -> Term.Set.t

val equates : t -> Term.t -> int -> bool
(** Does a stack struct holds a term with a val? *)

val subsumed : t -> t -> bool
(** [subsumed rho rho'] is true iff uf' |- uf using the normal equality rules. *)

(*val unify_partial : ?inverse:bool -> t Term.unifier *)
(** [unify_partial Option.some (Term.empty_subst, ()) u u'] computes a
    substitution [theta] such that [u'] |- [u[theta]].
    If the optional argument [~inverse:false] is set to [true] then a substitution
    is computed such that [u'[theta]] |- [u]. *)
