(** ASL formula, basically a list of symbolic heaps, denoting their disjunction. 
    NB no attempt to enforce variable or tag hygiene is made.  For example,
    if [f] and [g] both use an existential variable [x'] then [[f;g]] would
    mean the bound variable is shared. *)

include Util.BasicType with type t = Asl_heap.t list

val empty : t
(** The formula [emp]. NB this is not the same as [[]], which is equivalent to 
    false. *)

exception Not_symheap
val is_symheap : t -> bool
(** Returns true iff the formula has a single disjunct only *)
val dest : t -> Asl_heap.t
(** Return the single disjunct, if there is exactly one, else raise [Not_symheap]. *)

val equal_upto_tags : t -> t -> bool
(** Whilst [equal] demands syntactic equality including tags, this version 
    ignores tag assignment. *) 

val to_melt : t -> Latex.t
val terms : t -> Asl_term.Set.t
val vars : t -> Asl_term.Set.t

val inconsistent : t -> bool
(** Do all disjuncts entail false in the sense of [Asl_heap.inconsistent]? *)

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

val parse : ?null_is_emp:bool -> (t, 'a) MParser.t
val of_string : ?null_is_emp:bool -> string -> t

val star : t -> t -> t
(** Star two formulas by distributing [*] through [\/]. *)

val disj : t -> t -> t
(** Or two formulas (list-append). *)

val subst : Asl_subst.t -> t -> t

val subst_existentials : t -> t
(** Like [Asl_heap.subst_existentials] applied to all disjuncts. *)

val simplify_terms : t -> t

val norm : t -> t
(** Replace all terms with their UF representatives in the respective heaps. *)
 
