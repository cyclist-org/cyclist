(** Symbolic heaps. *)

type abstract1
type abstract2

type symheap = private {
  eqs : Sl_uf.t;
  deqs : Sl_deqs.t;
  ptos : Sl_ptos.t;
  inds : Sl_tpreds.t;
  mutable _terms : abstract1;
  mutable _vars : abstract1;
  mutable _tags : abstract2
}

include Utilsigs.BasicType with type t = symheap

val empty : t

(** Accessor functions. *)

val vars : t -> Sl_term.Set.t
val terms : t -> Sl_term.Set.t

val tags : t -> Tags.t
(** Return set of tags assigned to predicates in heap. *)

val tag_pairs : t -> Tagpairs.t
(** Return a set of pairs representing the identity function over the tags 
    of the formula.  This is to be used (as the preserving tag pairs) 
    whenever the inductive predicates 
    of a formula (in terms of occurences) are untouched by an inference rule.
    This includes rules of substitution, rewriting under equalities and 
    manipulating all other parts apart from [inds]. 
*) 

val to_melt : t -> Latex.t

val has_untagged_preds : t -> bool

val complete_tags : Tags.t -> t -> t
(** [complete_tags exist ts h] returns the symbolic heap obtained from [h] 
    by assigning all untagged predicates a fresh existential tag avoiding 
    those in [ts].
*)

val equates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a symbolic heap entail the equality of two terms? *)

val disequates : t -> Sl_term.t -> Sl_term.t -> bool
(** Does a symbolic heap entail the disequality of two terms? *)

val find_lval : Sl_term.t -> t -> Sl_pto.t option
(** Find pto whose address is provably equal to given term. *)

val idents : t -> Sl_predsym.MSet.t
(** Get multiset of predicate identifiers. *)

val inconsistent : t -> bool
(** Trivially false if heap contains t!=t for any term t, or if x=y * x!=y 
    is provable for any x,y.
    NB only equalities and disequalities are used for this check.
*)

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

val parse : ?allow_tags:bool -> ?augment_deqs:bool -> (t, 'a) MParser.t
val of_string : string -> t

val mk_pto : Sl_pto.t -> t
val mk_eq : Sl_tpair.t -> t
val mk_deq : Sl_tpair.t -> t
val mk_ind : Sl_tpred.t -> t

val mk : Sl_uf.t -> Sl_deqs.t -> Sl_ptos.t -> Sl_tpreds.t -> t
val dest : t -> (Sl_uf.t * Sl_deqs.t * Sl_ptos.t * Sl_tpreds.t)

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

val proj_sp : t -> t
val proj_pure : t -> t

val explode_deqs : t -> t

val star : ?augment_deqs:bool -> t -> t -> t
val diff : t -> t -> t

val fixpoint : (t -> t) -> t -> t

val subst : Sl_subst.t -> t -> t

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

val subst_tags : Tagpairs.t -> t -> t
(** Substitute tags according to the function represented by the set of 
    tag pairs provided. *)

val unify_partial : 
  ?tagpairs:bool -> ?update_check:Sl_unify.Unidirectional.update_check 
    -> t Sl_unify.Unidirectional.unifier
(** Unify two heaps such that the first becomes a subformula of the second. *)

val biunify_partial : 
  ?tagpairs:bool -> ?update_check:Sl_unify.Bidirectional.update_check 
    -> t Sl_unify.Bidirectional.unifier

val classical_unify : ?inverse:bool -> ?tagpairs:bool -> 
  ?update_check:Sl_unify.Unidirectional.update_check 
    -> t Sl_unify.Unidirectional.unifier
(** Unify two heaps, by using [unify_partial] for the pure (classical) part whilst
    using [unify] for the spatial part.
- If the optional argument [~inverse=false] is set to [true] then compute the 
  required substitution for the *second* argument as opposed to the first. *)

val classical_biunify : 
  ?tagpairs:bool -> ?update_check:Sl_unify.Bidirectional.update_check 
    -> t Sl_unify.Bidirectional.unifier

val norm : t -> t
(** Replace all terms with their UF representative (the UF in the heap). *)

val all_subheaps : t -> t list
(** [all_subheaps h] returns a list of all the subheaps of [h]. These are
    constructed by taking:
      - all the subsets of the disequalities of [h];
      - all the subsets of the points-tos of [h];
      - all the subsets of the predicates of [h];
      - the equivalence classes of each subset of variables in the equalities of
        [h] that also respect the equalities of [h] are constructed - this is
        done by using [Sl_uf.remove] to remove subsets of variables from 
        [h.eqs];
    and forming all possible combinations *)

val memory_consuming : t -> bool
(** [memory_consuming h] returns [true] iff whenever there is an inductive
    predicate in [h] there is also a points-to. **)

val constructively_valued : t -> bool
(** [constructively_valued h] returns true if all variables in [h] are
    c.valued.  A variable [v] in [h] is c.valued iff (recursively) 
    - it is free, or,
    - there is a c.valued variable [v'] such that 
      - [v=v'] is in [h], or,
      - [v' |-> ys] is in [h] and [v] appears in [ys]. **)
