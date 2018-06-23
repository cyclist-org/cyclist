(** A structure representing a set of constraints on ordinal-valued variables.
    The structure may be queried to test whether the constraint set entails
    particular relationships between pairs of ordinal variables.
 *)

module Elt : sig
  include Utilsigs.BasicType
  val tags : t -> Tags.t
  val subst_tags : Tagpairs.t -> t -> t
  val satisfiable : t Fun.predicate
  val valid : t Fun.predicate
  val unify :
    ?update_check:((Tagpairs.t Unification.state_update) Fun.predicate) ->
      (Tagpairs.t, 'a, t) Unification.cps_unifier
  val biunify :
    ?update_check:
      ( ((Tagpairs.t * Tagpairs.t) Unification.state_update)
          Fun.predicate ) ->
      ((Tagpairs.t * Tagpairs.t), 'a, t) Unification.cps_unifier
end

include Utilsigs.OrderedContainer with type elt = Elt.t

val inconsistent : t -> bool

val tags : t -> Tags.t

val tag_pairs : t -> Tagpairs.t
(** Return a set of pairs representing the identity function over the tags
    in the constraint set, to be used as preserving tag pairs.
*)

val subst_tags : Tagpairs.t -> t -> t

val generate : ?avoid:Tags.t -> ?augment:bool
  -> Tags.elt -> Tags.t -> t
(** [generate avoid t ts] returns a constraint set that constitutes an inductive
    hypothesis corresponding to a case in the unfolding of a predicate tagged
    with [t] that recursively depends on predicate instances tagged by labels
    in [ts]; any freshly generated labels are not contained in [avoid]. If the
    optional argumet [augment=true] is set to [false] then, no constraints will
    be generated in the case that [ts] is empty.
*)

val remove_schema : t -> Tags.t -> (t * string) option
(** [remove_schema cs used] will remove a (nonempty) subset [cs'] of [cs]
    containing tags \{ t_1, ..., t_n \} where at least one t_i does not occur in
    [used] such that [cs'] does not affect the support of [cs] in the sense that

      [cs''] |= [cs] \ [cs'] if and only if [cs''] |= [cs]   for all [cs'']

    The remaining constraint set (i.e. [cs] \ [cs']) is returned along with a
    string that describes the removed set of constraints. If no such set can be
    found, then [None] is returned.

    Intuitively, this means identifying and removing a tautological schema
    bounded by some (set of) existential tag(s) (outside of the set [used]).
      For example, cs = \{ t_1 < t, ..., t_n < t \} + cs'' where t is an
    existential tag not occurring in cs'', picks out t as a strict upper bound
    of the tags t_1, ..., t_n; since such a bound always exists, this is a
    tautological schema.
*)

val verify_schemas : Tags.t -> t -> bool
(** [verify_schemas ts cs] will return [true] when [cs] consists entirely of
    tautological schemas. This is computed by repeatedly trying to remove
    schemas using [remove_schema cs ts] until failure; if [cs] before failure
    was empty, then [true] is returned, and [false] otherwise. *)

val upper_bounds : ?strict:bool -> Tags.elt -> t -> Tags.t
(** [upper_bounds ?strict t cs] returns the set of tags [b] such that [cs]
    contains a constraint of the form [t] <= [b]. If the option argument
    [strict=false] is set to [true] then the set of tags [b] suh that [cs]
    contains a constraints of the form [t] < [b] is returned. *)

val close : t -> t
(** [close cs] generates the set of all constraints entailed by [cs] *)

val all_pairs : t -> Tagpairs.t
(** [all_pairs cs] returns all of the tag pairs ([a], [b]) such that [a] <= [b]
    is entailed by some constraint in [cs] *)

val prog_pairs : t -> Tagpairs.t
(** [prog_pairs cs] returns all of the tag pairs ([a], [b]) such that [a] <[b]
    is entailed by some constraint in [cs] *)

val subsumes : t -> t -> bool
(** [subsumes cs cs'] checks whether every constraint in [cs'] also occurs in
    [cs], ignoring any constraints in [cs'] which are universally valid
    (e.g. t <= t)
*)

val unify :
  ?total:bool -> ?inverse:bool ->
    ?update_check:((Tagpairs.t Unification.state_update) Fun.predicate) ->
      (Tagpairs.t, 'a, t) Unification.cps_unifier
(** [unify check cs cs' cont init_state] calculates a (minimal) extension theta
    of [init_state] such that [subsumes cs' (subst_tags theta cs)] returns
    [true], then passes it to [cont] and returns the resulting (optional) value.
      If the value of the optional argument [total=false] is set to [true] then
    [subsumes (subst_tags theta cs) cs'] will be [true] of the result also.
      If the value of the optional argument [inverse=false] is set to [true]
    then theta is returned such that [subsumes (subst_tags theta cs') cs]
    is [true] instead.
*)

val biunify :
  ?total:bool ->
    ?update_check:
      ( ((Tagpairs.t * Tagpairs.t) Unification.state_update)
          Fun.predicate ) ->
      ((Tagpairs.t * Tagpairs.t), 'a, t) Unification.cps_unifier

val mk_update_check :
  (Tagpairs.t * (Tags.Elt.t * Tags.Elt.t)) Fun.predicate
    -> (Tagpairs.t * Tagpairs.t) Fun.predicate

val parse : (t, 'a) MParser.parser
val of_string : string -> t

val to_string_list : t -> string list

val to_string : t -> string
