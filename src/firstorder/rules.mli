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

val product_subsumed_upto_tags : Prod.t -> Prod.t -> bool
val ex_falso_axiom : Rule.t
val id_axiom : Rule.t
val axioms : Rule.t ref

val eq_subst_rule :
  Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val simplify_eqs :
  Seqtactics.seq_t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val bijections : Lib.Strng.Set.t

val bij_eqs :
  Form.t * 'a ->
  (((Form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string) list

val func_eqs :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val eq_ex_subst_rule :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val simpl_rhs :
  Form.t * Form.t ->
  (((Form.t * Form.t) * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val simplify_rules :
  (Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list)
  list

val simplify_seq : Seqtactics.t
val simplify : Rule.t
val wrap : Seqtactics.t -> Rule.t
val lhs_disj_to_products : Rule.t
val rhs_conj_to_atoms : Rule.t
val instantiate_ex : Rule.t
val matches_ident : string -> Prod.elt -> bool
val args_of_ipred : Prod.elt -> Term.t list
val gen_right_rules : string * Case.t list -> Rule.t list
val gen_left_rules : string * Case.t list -> Rule.t

val matches_fun :
  Seqtactics.seq_t ->
  Seqtactics.seq_t ->
  ((Generic.Tagpairs.t * string) * Term.substitution) list

val subst_rule :
  Term.substitution ->
  Seqtactics.seq_t ->
  Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val weaken :
  Seqtactics.seq_t ->
  Seqtactics.seq_t ->
  ((Seqtactics.seq_t * Generic.Tagpairs.t * Generic.Tagpairs.t) list * string)
  list

val dobackl : int -> Rule.proof_t -> (int list * Rule.proof_t) list
val fold : string * Case.t list -> Rule.t
val rules : Rule.t ref
val setup : Defs.t -> unit
