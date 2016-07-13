(** SL formula, basically a list of symbolic heaps, denoting their disjunction. 
    NB no attempt to enforce variable or tag hygiene is made.  For example,
    if [f] and [g] both use an existential variable [x'] then [[f;g]] would
    mean the bound variable is shared. *)

include Util.BasicType with type t = Sl_heap_rho.t list

val empty : t
(** The formula [emp]. NB this is not the same as [[]], which is equivalent to 
    false. *)

exception Not_symheap
val is_symheap : t -> bool
(** Returns true iff the formula has a single disjunct only *)
val dest : t -> Sl_heap_rho.t
(** Return the single disjunct, if there is exactly one, else raise [Not_symheap]. *)

val equal_upto_tags : t -> t -> bool
(** Whilst [equal] demands syntactic equality including tags, this version 
    ignores tag assignment. *) 

val to_melt : t -> Latex.t
val terms : t -> Sl_term.Set.t
val vars : t -> Sl_term.Set.t

val tags : t -> Util.Tags.t
(** NB no attempt is made to ensure that tags are disjoint between disjuncts. 
    This is not necessarily unsound. *)
val tag_pairs : t -> Util.TagPairs.t
(** The proviso on tags applies here too. *)

val inconsistent : t -> bool
(** Do all disjuncts entail false in the sense of [Sl_heap_rho.inconsistent]? *)

val subsumed : ?total:bool -> t -> t -> bool
(** [subsumed a b]: is it the case that for any disjunct [a'] of [a] there a 
    disjunct [b'] of [b] such [a'] is subsumed by [b']?
    If the optional argument [~total=true] is set to [false] then relax the 
    check on the spatial part so that it is included rather than equal to that
    of [b].
    NB this includes matching the tags exactly. *)   
val subsumed_upto_tags : ?total:bool -> t -> t -> bool
(** As above but ignoring tags.
    If the optional argument [~total=true] is set to [false] then relax the 
    check on the spatial part so that it is included rather than equal to that
    of [b]. *)

val parse : (t, 'a) MParser.t
val of_string : string -> t

val star : t -> t -> t
(** Star two formulas by distributing [*] through [\/]. *)

val disj : t -> t -> t
(** Or two formulas (list-append). *)

val subst : Sl_subst.t -> t -> t

val subst_existentials : t -> t
(** Like [Sl_heap_rho.subst_existentials] applied to all disjuncts. *)

val subst_tags : Util.TagPairs.t -> t -> t
(** Like [Sl_heap_rho.subst_tags] applied to all disjuncts. *)

val norm : t -> t
(** Replace all terms with their UF representatives in the respective heaps. *)
 
