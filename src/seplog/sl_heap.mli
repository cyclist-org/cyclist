(** Symbolic heaps. *)

type symheap = private {
  eqs : Sl_uf.t;
  deqs : Sl_deqs.t;
  ptos : Sl_ptos.t;
  inds : Sl_tpreds.t;
}

include Util.BasicType with type t = symheap

val empty : t

(** Accessor functions. *)

val vars : t -> Sl_term.Set.t
val terms : t -> Sl_term.Set.t

val tags : t -> Util.Tags.t
(** Return set of tags assigned to predicates in heap. *)

val tag_pairs : t -> Util.TagPairs.t
(** Return a set of pairs representing the identity function over the tags 
    of the formula.  This is to be used (as the preserving tag pairs) 
    whenever the inductive predicates 
    of a formula (in terms of occurences) are untouched by an inference rule.
    This includes rules of substitution, rewriting under equalities and 
    manipulating all other parts apart from [inds]. 
*) 

val to_melt : t -> Latex.t

val equates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a symbolic heap entail the equality of two terms? *)

val disequates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a symbolic heap entail the disequality of two terms? *)

val find_lval : Sl_term.t -> t -> Sl_pto.t option
(** Find pto whose address is provably equal to given term. *)

val idents : t -> Util.Strng.MSet.t
(** Get multiset of predicate identifiers. *)

val inconsistent : t -> bool
(** Trivially false if x=y * x!=y is provable for any x,y.
    NB only equalities and disequalities are used for this check.
*)

val subsumed : t -> t -> bool
(** [subsumed h h'] is true iff [h] can be rewritten using the equalities
    in [h'] such that its spatial part becomes equal to that of [h']
    and the pure part becomes a subset of that of [h']. *)

val subsumed_upto_tags : t -> t -> bool
(** Like [subsumed] but ignoring tag assignment. *)

val equal_upto_tags : t -> t -> bool
(** Like [equal] but ignoring tag assignment. *)

(** Constructors. *)

val parse : (t, 'a) MParser.t
val of_string : string -> t

val mk_pto : Sl_pto.t -> t
val mk_eq : Sl_tpair.t -> t
val mk_deq : Sl_tpair.t -> t
val mk_ind : Sl_tpred.t -> t

val mk : Sl_uf.t -> Sl_deqs.t -> Sl_ptos.t -> Sl_tpreds.t -> t

(** Functions [with_*] accept a heap [h] and a heap component [c] and 
    return the heap that results by replacing [h]'s appropriate component 
    with [c]. *)

val with_eqs : t -> Sl_uf.t -> t 
val with_deqs : t -> Sl_deqs.t -> t
val with_ptos : t -> Sl_ptos.t -> t
val with_inds : t -> Sl_tpreds.t -> t

val del_deq : t -> Sl_tpair.t -> t
val del_pto : t -> Sl_pto.t -> t
val del_ind : t -> Sl_tpred.t -> t

val add_eq : t -> Sl_tpair.t -> t
val add_deq : t -> Sl_tpair.t -> t
val add_pto : t -> Sl_pto.t -> t
val add_ind : t -> Sl_tpred.t -> t

val star : t -> t -> t
val fixpoint : (t -> t) -> t -> t

val subst : Sl_term.substitution -> t -> t

val univ : Sl_term.Set.t -> t -> t
(** Replace all existential variables with fresh universal variables. *)

val subst_existentials : t -> t
(** For all equalities x'=t, remove the equality and do the substitution [t/x'] *)

val project : t -> Sl_term.t list -> t
(** See CSL-LICS paper for explanation.  Intuitively, do existential 
    quantifier elimination for all variables not in parameter list. *)  

val freshen_tags : t -> t -> t
(** [freshen_tags f g] will rename all tags in [g] such that they are disjoint
    from those of [f]. *)

val subst_tags : Util.TagPairs.t -> t -> t
(** Substitute tags according to the function represented by the set of 
    tag pairs provided. *)

val unify_within : (t, 'a) Sl_term.unifier
(** Unify two heaps such that the first becomes a subformula of the second. *)

val classical_unify : (t, 'a) Sl_term.unifier
(** Unify two heaps, by using [unify_within] for the pure (classical) part whilst
    using [unify] for the spatial part. *)

val inverse_classical_unify : (t, 'a) Sl_term.unifier
(** Unify two heaps as in [classical_unify] but apply the substitution to the 
    *second* argument as opposed to the first. *)

val tagged_classical_unify : (t, Util.TagPairs.t) Sl_term.unifier
(** In addition to the substitution found by [classical_unify], 
    return the set of pairs of tags of predicates unified. *)


