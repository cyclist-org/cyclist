(** Symbolic heaps. *)

type abstract1

type symheap = private {
  eqs : Asl_uf.t;
  neqs : Asl_neqs.t;
  leqs : Asl_leqs.t;
  lts : Asl_lts.t;
  arrays : Asl_arrays.t;
  mutable _terms : Asl_term.Set.t option;
  mutable _vars : Asl_term.Set.t option
}

include Util.BasicType with type t = symheap

val empty : t

(** Accessor functions. *)

val free_vars : t -> Asl_term.Set.t
val vars : t -> Asl_term.Set.t
val terms : t -> Asl_term.Set.t


val to_melt : t -> Latex.t


val equates : t -> Asl_term.t -> Asl_term.t -> bool
(** Does a symbolic heap entail the equality of two terms? *)

val disequates : t -> Asl_term.t -> Asl_term.t -> bool
(** Does a symbolic heap entail the disequality of two terms? *)

val entail_le : t -> Asl_term.t -> Asl_term.t -> bool
val entail_lt : t -> Asl_term.t -> Asl_term.t -> bool


val subsumed : ?total:bool -> t -> t -> bool
(** [subsumed h h'] is true iff [h] can be rewritten using the equalities
    in [h'] such that its spatial part becomes equal to that of [h']
    and the pure part becomes a subset of that of [h']. 
    If the optional argument [~total=true] is set to [false] then check whether
    both pure and spatial parts are subsets of those of [h'] modulo the equalities
    of [h']. *)

val subsumed_upto_tags : ?total:bool -> t -> t -> bool
(** Like [subsumed] but ignoring tag assignment.
    If the optional argument [~total=true] is set to [false] then check whether
    both pure and spatial parts are subsets of those of [h'] modulo the equalities
    of [h']. *)

val equal : t -> t -> bool
(** Checks whether two symbolic heaps are equal. *)
val equal_upto_tags : t -> t -> bool
(** Like [equal] but ignoring tag assignment. *)
val is_empty : t -> bool
(** [is_empty h] tests whether [h] is equal to the empty heap. *)

(** Constructors. *)

val parse : (t, 'a) MParser.t
val of_string : string -> t

val mk_arr : Asl_array.t -> t
val mk_eq : Asl_tpair.t -> t
val mk_neq : Asl_tpair.t -> t

val mk : Asl_uf.t -> Asl_neqs.t -> Asl_leqs.t -> Asl_lts.t -> Asl_arrays.t -> t
val dest : t -> (Asl_uf.t * Asl_neqs.t * Asl_leqs.t * Asl_lts.t * Asl_arrays.t)

(* val combine : t -> t -> t *)

(** Functions [with_*] accept a heap [h] and a heap component [c] and 
    return the heap that results by replacing [h]'s appropriate component 
    with [c]. *)

val with_eqs : t -> Asl_uf.t -> t 
val with_neqs : t -> Asl_neqs.t -> t
val with_leqs : t -> Asl_leqs.t -> t
val with_lts : t -> Asl_lts.t -> t
val with_arrays : t -> Asl_arrays.t -> t

val del_neq : t -> Asl_tpair.t -> t
val del_arr : t -> Asl_array.t -> t

val add_eq : t -> Asl_tpair.t -> t
val add_neq : t -> Asl_tpair.t -> t
val add_leq : t -> Asl_tpair.t -> t
val add_lt : t -> Asl_tpair.t -> t
val add_arr : t -> Asl_array.t -> t

val star : t -> t -> t

val subst : Asl_subst.t -> t -> t

val subst_existentials : t -> t
(** For all equalities x'=t, remove the equality and do the substitution [t/x'] *)

val norm : t -> t
(** Replace all terms with their UF representative (the UF in the heap). *)

val unify_partial : ?tagpairs:bool -> t Asl_unifier.t
(** Unify two heaps such that the first becomes a subformula of the second.
- If the optional argument [~tagpairs=false] is set to [true] then in addition 
  to the substitution found, also return the set of pairs of tags of 
  predicates unified. *)

val satisfiable_z3 : ?pure:Fopl.formula option -> t -> string
(** \exists FV(heap) s.t. gamma(heap), where gamma is a construction
 * defined in the paper. *)

val pure_to_fopl : t -> Fopl.formula
(** Convert pure part of symb heap to FOPL formula. *)

val simplify_terms: t -> t
(** For all terms in the heap substitutues with Asl_term.simplify counterpart. *)
