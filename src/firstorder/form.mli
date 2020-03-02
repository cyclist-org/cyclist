open Lib
open Generic

include OrderedContainer with type elt = Prod.t

val dest : t -> Prod.t
val tags : t -> Tags.t
val tag_pairs : t -> Tagpairs.t
val pp : Format.formatter -> t -> unit
val is_prod : t -> bool

val terms : t -> Term.Set.t

val equal_upto_tags  : t -> t -> bool
val subsumed_wrt_tags : Tags.t -> t -> t -> bool

val subst : Term.substitution -> t -> t

val uni_subsumption :
  bool -> (Term.substitution -> 'a option) ->
    Term.substitution -> t -> t -> 'a option
val left_subsumption :
  (Term.substitution -> 'a option) -> Term.substitution -> t -> t -> 'a option
val right_subsumption :
  (Term.substitution -> 'a option) -> Term.substitution -> t -> t -> 'a option

val parse : (t, 'a) MParser.t