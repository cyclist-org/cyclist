(** Inductive rule type consisting of a symbolic heap and a predicate head. *)

include Utilsigs.BasicType

val mk : Sl_heap.t -> Sl_pred.t -> t
val dest: t -> Sl_heap.t * Sl_pred.t

val vars : t -> Sl_term.Set.t

val predsym : t -> Sl_predsym.t
val arity : t -> int
val formals : t -> Sl_term.t list
val body : t -> Sl_heap.t

val freshen : Sl_term.Set.t -> t -> t
(** Replace all variables in rule such that they are disjoint with the set 
    provided. *)

val subst : Sl_subst.t -> t -> t
val parse : (t, 'a) MParser.t

val unfold : 
  ?gen_tags:bool -> (Sl_term.Set.t * Tags.t) -> Sl_tpred.t -> t -> Sl_heap.t
(** [unfold (vs, ts) p r] returns the body of the inductive rule [r] with:
      the formal parameters replaced by the arguments of [p]; 
      the remaining variables freshened, avoiding those in [vs]; and
      the predicates assigned fresh existential tags avoiding those in [ts],
        unless the optional argument [gen_tags=true] is set to false.
    NB. This assumes that all predicates in the body of [r] are untagged.
*)  
    
val fold : t -> Sl_heap.t -> (Sl_subst.t * Sl_heap.t) list
(** [fold r h] returns a list of pairs of substitutions over the formal parameters of 
    the rule [r] such that its body, when one of these substitutions is applied,
    becomes a subformula of [h]; and the result of removing that subformula from [h].
    NB the pure part is removed on a best effort basis. *)

val memory_consuming : t -> bool
(** [memory_consuming r] returns [true] the body of [r] is memory consuming 
    (see [Sl_heap]). **)

val constructively_valued : t -> bool
(** [constructively_valued r] returns true iff the body of [r] is 
    constructively valued (see [Sl_heap]).  **)
