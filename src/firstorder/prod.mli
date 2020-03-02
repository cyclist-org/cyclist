open Lib
open Generic

include OrderedContainer with type elt = Atom.t

include Containers.S
  with type Set.elt = t
  with type Map.key = t
  with type Hashmap.key = t
  with type Hashset.elt = t
  with type MSet.elt = t
  with type FList.t = t list

val get_eqs : t -> (Term.t * Term.t) list
val get_deqs : t -> (Term.t * Term.t) list
val filter_by_kind : Atom.t -> t -> t

val tags : t -> Tags.t
val terms : t -> Term.Set.t
val univ : Term.Set.t -> t -> t
val repl_tags : Tags.Elt.t -> t -> t
val subsumed_wrt_tags : Tags.t -> t -> t -> bool
val tag_pairs : t -> Tagpairs.t
val subst : Term.substitution -> t -> t
(* val to_string : t -> string
val pp : Format.formatter -> t -> unit *)
val left_subsumption :
  (Term.substitution -> Term.substitution option) ->
    Term.substitution -> t -> t -> Term.substitution option

val equal_upto_tags : t -> t -> bool
val subsumed_wrt_tags : Tags.t -> t -> t -> bool

val uni_subsumption :
  bool -> (Term.substitution -> 'a option) ->
    Term.substitution -> t -> t -> 'a option
val left_subsumption :
  (Term.substitution -> 'a option) -> Term.substitution -> t -> t -> 'a option

val parse : (t, 'a) MParser.t

exception Not_product