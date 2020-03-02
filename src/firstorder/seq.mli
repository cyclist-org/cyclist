open Lib
open Generic

type t = Form.t * Form.t

val equal : t -> t -> bool
val equal_upto_tags : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val of_string : string -> t

val tags : t -> Tags.t
val dest : t -> Prod.t * Prod.t
val terms : t -> Term.Set.t
val vars : t -> Term.Set.t
val tag_pairs : t -> Tagpairs.t
val uni_subsumption : t -> t -> Term.substitution option
val subsumed_wrt_tags : Tags.t -> t -> t -> bool
val subst : Term.substitution -> t -> t
