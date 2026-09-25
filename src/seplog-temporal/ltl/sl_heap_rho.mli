(** Symbolic heaps. *)
open Lib
open Generic
open Seplog


type abstract1
type abstract2

type symheap = private {
	rho : Sl_rho.t;
  eqs : Uf.t;
  deqs : Deqs.t;
  ptos : Ptos.t;
  inds : Tpreds.t;
  mutable _terms : abstract1;
  mutable _vars : abstract1;
  mutable _tags : abstract2
}

include BasicType with type t = symheap

val empty : t

(** Accessor functions. *)

val vars : t -> Term.Set.t
val terms : t -> Term.Set.t

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

val has_untagged_preds : t -> bool
(** [has_untagged_pred h] returns [true] iff [h] contains untagged predicates. *)

val complete_tags : Tags.t -> t -> t
(** [complete_tags exist ts h] returns the symbolic heap obtained from [h]
    by assigning all untagged predicates a fresh existential tag avoiding
    those in [ts].
*)

val equates : t -> Term.t -> Term.t -> bool
(** Does a symbolic heap entail the equality of two terms? *)

val disequates : t -> Term.t -> Term.t -> bool
(** Does a symbolic heap entail the disequality of two terms? *)

val find_lval : Term.t -> t -> Pto.t option
(** Find pto whose address is provably equal to given term. *)

val idents : t -> Predsym.MSet.t
(** Get multiset of predicate identifiers. *)

val inconsistent : t -> bool
(** Trivially false if x=y * x!=y is provable for any x,y.
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

val parse : (t, 'a) MParser.t
val of_string : string -> t

val mk_rho : Term.t * int -> t
val mk_pto : Pto.t -> t
val mk_eq : Tpair.t -> t
val mk_deq : Tpair.t -> t
val mk_ind : Tpred.t -> t

val mk : Sl_rho.t -> Uf.t -> Deqs.t -> Ptos.t -> Tpreds.t -> t
val dest : t -> (Sl_rho.t * Uf.t * Deqs.t * Ptos.t * Tpreds.t)

val combine : t -> t -> t

(** Functions [with_*] accept a heap [h] and a heap component [c] and
    return the heap that results by replacing [h]'s appropriate component
    with [c]. *)

val with_rho : t -> Sl_rho.t -> t
val with_eqs : t -> Uf.t -> t
val with_deqs : t -> Deqs.t -> t
val with_ptos : t -> Ptos.t -> t
val with_inds : t -> Tpreds.t -> t

val del_deq : t -> Tpair.t -> t
val del_pto : t -> Pto.t -> t
val del_ind : t -> Tpred.t -> t

val add_eq : t -> Term.t -> int -> t
val add_eq : t -> Tpair.t -> t
val add_deq : t -> Tpair.t -> t
val add_pto : t -> Pto.t -> t
val add_ind : t -> Tpred.t -> t

val proj_sp : t -> t
val proj_pure : t -> t

val star : t -> t -> t
val diff : t -> t -> t

val fixpoint : (t -> t) -> t -> t

val subst : Term.Subst.t -> t -> t

val univ : Term.Set.t -> t -> t
(** Replace all existential variables with fresh universal variables. *)

val subst_existentials : t -> t
(** For all equalities x'=t, remove the equality and do the substitution [t/x'] *)

val project : t -> Term.t list -> t
(** See CSL-LICS paper for explanation.  Intuitively, do existential
    quantifier elimination for all variables not in parameter list. *)

val freshen_tags : t -> t -> t
(** [freshen_tags f g] will rename all tags in [g] such that they are disjoint
    from those of [f]. *)

val subst_tags : Tagpairs.t -> t -> t
(** Substitute tags according to the function represented by the set of
    tag pairs provided. *)

val unify_partial :
      ?tagpairs:bool ->
        ?update_check:Unify.Unidirectional.update_check ->
          t Unify.Unidirectional.unifier
(** Unify two heaps such that the first becomes a subformula of the second.
- If the optional argument [~tagpairs=false] is set to [true] then in addition
  to the substitution found, also return the set of pairs of tags of
  predicates unified. *)

val classical_unify :
      ?inverse:bool -> ?tagpairs:bool ->
        ?update_check:Unify.Unidirectional.update_check ->
          t Unify.Unidirectional.unifier
(** Unify two heaps, by using [unify_partial] for the pure (classical) part whilst
    using [unify] for the spatial part.
- If the optional argument [~inverse=false] is set to [true] then compute the
  required substitution for the *second* argument as opposed to the first.
- If the optional argument [~tagpairs=false] is set to [true] then in addition
  to the substitution found, also return the set of pairs of tags of
  predicates unified. *)

val compute_frame :
      ?freshen_existentials:bool -> ?avoid:Term.Set.t -> t -> t -> t option
(** [compute_frame f f'] computes the portion of [f'] left over (the 'frame')
    after subtracting all the atomic formulae in the specification [f]. Returns
    None when there are atomic formulae in [f] which are not also in [f'] (i.e.
    [f] is not subsumed by [f']). Any existential variables occurring in the
    frame which also occur in the specification [f] are freshened, avoiding the
    variables in the optional argument [~avoid=Term.Set.empty].
- If the optional argument [~freshen_existentials=true] is set to false, then
  None will be returned in case there are existential variables in the frame
  which also occur in the specification. *)

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
        done by using [Uf.remove] to remove subsets of variables from
        [h.eqs];
    and forming all possible combinations *)
