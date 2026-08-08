module SH = Heap

exception Not_symheap

module Proof :
  Generic.Proof.S
    with type t = Generic.Proof.Make(Seq).t
     and type seq_t = Seq.t
     and type node_t = Generic.Proofnode.Make(Seq).t

module Rule :
  Generic.Proofrule.S
    with type seq_t = Seq.t
     and type proof_t = Generic.Proof.Make(Seq).t

module Seqtactics : Generic.Seqtactics.S with type seq_t = Seq.t

val set_default_select_f : int -> unit
val default_select_f_descr : ?line_prefix:string -> unit -> string

type t_lemma_level = NO_LEMMAS | ONLY_WITH_PREDICATES | NON_EMPTY | ANY

val lemma_equal : t_lemma_level -> t_lemma_level -> bool
val lemma_level : t_lemma_level ref
val set_lemma_level : int -> unit
val lemma_option_descr_str : ?line_prefix:string -> unit -> string
val id_axiom : Rule.t
val preddefs : Defs.t ref
val ex_falso_axiom : Rule.t
val lhs_disj_to_symheaps : Rule.t

val rhs_disj_to_symheaps_rl :
  (Generic.Ord_constraints.t * SH.t list)
  * (Generic.Ord_constraints.t * SH.t list) ->
  ((((Generic.Ord_constraints.t * SH.t list) * Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val rhs_disj_to_symheaps : Rule.t

val lhs_instantiate_ex_tags :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val lhs_instantiate_ex_vars :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val lhs_instantiation_rules :
  (Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list)
  list

val lhs_instantiate_seq : Seqtactics.t
val lhs_instantiate : Rule.t

val eq_subst_rule :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val eq_ex_subst_rule :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val eq_simplify :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val deq_simplify :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val constraint_simplify :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val norm :
  Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val simplify_rules :
  (Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list)
  list

val simplify_seq : Seqtactics.t
val simplify : Rule.t
val wrap : Seqtactics.t -> Rule.t
val pto_intro_rule : Rule.t
val pred_intro_rule : Rule.t
val instantiate_pto : Rule.t
val constraint_match_tag_instantiate : Rule.t
val upper_bound_tag_instantiate : Rule.t

val bounds_intro_rl :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val bounds_intro : Rule.t

val ruf_rl :
  Defs.t ->
  Seqtactics.seq_t ->
  ((((Generic.Ord_constraints.t * SH.t list)
    * (Generic.Ord_constraints.t * SH.t list))
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val ruf : Defs.t -> Rule.t
val luf : Defs.t -> Rule.t

val matches :
  Form.t * Form.t -> Form.t * Form.t -> Unify.Unidirectional.state list

val subst_rule :
  Subst.t * Generic.Tagpairs.t ->
  Form.t * Form.t ->
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val weaken :
  Seqtactics.seq_t ->
  Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val left_transform_rule :
  Form.t * Form.t ->
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val right_transform_rule :
  Form.t * Form.t ->
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val apply_lemma :
  Seqtactics.seq_t * (Form.t * Form.t) ->
  Form.t * Form.t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val mk_backlink_rule_seq :
  Subst.t * Generic.Tagpairs.t ->
  Form.t * Form.t ->
  int * Seqtactics.seq_t ->
  Rule.t

val mk_lemma_rule_seq :
  Subst.t * Generic.Tagpairs.t ->
  Form.t * Form.t ->
  int * (Form.t * Form.t) ->
  Rule.t

type backlink_t = FULL of Rule.t | PARTIAL of Rule.t

val dest_taggedrule : backlink_t -> Rule.t
val cmp_taggedrule : backlink_t -> backlink_t -> int
val dobackl : int -> Rule.proof_t -> (int list * Rule.proof_t) list
val axioms : Rule.t ref
val rules : Rule.t ref
val use_invalidity_heuristic : bool ref
val setup : Defs.t -> unit
