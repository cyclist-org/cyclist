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

(** Constructors. *)

val parse : (t, 'a) MParser.t
val of_string : string -> t

val mk_pto : Sl_pto.t -> t
val mk_eq : Sl_tpair.t -> t
val mk_deq : Sl_tpair.t -> t
val mk_ind : Sl_tpred.t -> t

val mk : Sl_uf.t -> Sl_deqs.t -> Sl_ptos.t -> Sl_tpreds.t -> t

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

val part_unify : t Sl_term.gen_unifier
val tagged_part_unify : t Sl_tpreds.tagged_unifier
(** Part unify a heap with another and return in addition to the substitution,
    the set of pairs of tags of predicates unified. *)
    


