(** Point
s-to atom, consisting of a pair of a term and a list of terms. *)
open Generic
 

include Lib.BasicType with type t = Term.t * Term.FList.t
(* include Lib.BasicType with type t = Permissions.t * (Term.t * Term.FList.t) *)
(* include Lib.BasicType with type t = (Permissions.t * Tags.Elt.t) * (Term.t * Term.FList.t)  *)

val subst : Subst.t -> t -> t

val terms : t -> Term.Set.t

val vars : t -> Term.Set.t

val parse : (t, 'a) MParser.parser

(*
  In your procedure precondition you have x |-> y
  
  and in your current state at the call site you have z |-> nil and you have to
  match them up,

*)
val unify :
     ?update_check:Unify.Unidirectional.update_check
  -> t Unify.Unidirectional.unifier
(** Compute substitution that unifies two points-tos. *)

val biunify :
     ?update_check:Unify.Bidirectional.update_check
  -> t Unify.Bidirectional.unifier

val norm : Uf.t -> t -> t
(** Replace all terms with their UF representative. NB this may replace [nil]
    with a variable. *)

val record_type : t -> Term.t * int
