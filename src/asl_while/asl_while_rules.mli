module SH = Asl.Asl_heap

exception Not_symheap

module Rule :
  Generic.Proofrule.S
    with type seq_t = Asl_while_program.Seq.t
     and type proof_t = Generic.Proof.Make(Asl_while_program.Seq).t

module Seqtactics :
  Generic.Seqtactics.S with type seq_t = Asl_while_program.Seq.t

module Proof :
  Generic.Proof.S
    with type t = Generic.Proof.Make(Asl_while_program.Seq).t
     and type seq_t = Asl_while_program.Seq.t
     and type node_t = Generic.Proofnode.Make(Asl_while_program.Seq).t

module Node :
  Generic.Proofnode.S
    with type t = Generic.Proofnode.Make(Asl_while_program.Seq).t
     and type seq_t = Asl_while_program.Seq.t

val tagpairs : 'a -> Generic.Tagpairs.t
val dest_sh_seq : Asl.Asl_form.t * 'a -> SH.t * 'a
val ex_falso_axiom : Rule.t
val symex_stop_axiom : Rule.t
val symex_empty_axiom : Rule.t

val eq_subst_ex_f :
  Asl.Asl_form.t * 'a ->
  (((Asl.Asl_form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list

val simplify_rules :
  (Asl.Asl_form.t * 'a ->
  (((Asl.Asl_form.t * 'a) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list)
  list

val simplify_seq_rl : Seqtactics.t
val simplify : Rule.t
val wrap : Seqtactics.t -> Rule.t
val lhs_disj_to_symheaps : Rule.t

val fix_tps :
  ('a list * 'b) list ->
  (('a * Generic.Tagpairs.t * Generic.Tagpairs.t) list * 'b) list

val mk_symex :
  (Asl.Asl_form.t * Asl_while_program.Cmd.t -> (SH.t list * string) list) ->
  Rule.t

val symex_assign_rule : Rule.t
val find_arr_on : SH.t -> Asl.Asl_term.term_t -> Asl.Asl_arrays.elt
val join_arrays : SH.t -> Asl.Asl_arrays.elt -> SH.t

val add_or_lt :
  SH.t ->
  Asl.Asl_term.term_t * Asl.Asl_term.term_t ->
  Asl.Asl_term.term_t * Asl.Asl_term.term_t ->
  Asl.Fopl.formula option

val add_or_le :
  SH.t ->
  Asl.Asl_term.term_t * Asl.Asl_term.term_t ->
  Asl.Asl_term.term_t * Asl.Asl_term.term_t ->
  Asl.Fopl.formula option

val symex_free_rule : Rule.t
val symex_store_rule : Rule.t
val symex_load_rule : Rule.t
val symex_new_rule : Rule.t
val symex_skip_rule : Rule.t
val symex_if_rule : Rule.t
val symex_ifelse_rule : Rule.t
val symex_while_rule : Rule.t

val matches :
  Asl.Asl_form.t * Asl_while_program.Cmd.t ->
  Asl.Asl_form.t * Asl_while_program.Cmd.t ->
  Asl.Asl_unifier.state list

val subst_rule :
  Asl.Asl_subst.t ->
  Asl.Asl_form.t * Asl_while_program.Cmd.t ->
  Asl.Asl_form.t * Asl_while_program.Cmd.t ->
  (((Asl.Asl_form.t * Asl_while_program.Cmd.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val frame :
  Asl.Asl_form.t * Asl_while_program.Cmd.t ->
  Asl.Asl_form.t * Asl_while_program.Cmd.t ->
  (((Asl.Asl_form.t * Asl_while_program.Cmd.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val dobackl : int -> Proof.t -> (int list * Proof.t) list
val generalise_while_rule : int -> Proof.t -> (int list * Proof.t) list
val axioms : Rule.t ref
val rules : Rule.t ref
val symex : Rule.t
val setup : unit -> unit
