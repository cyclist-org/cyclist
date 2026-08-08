exception Not_symheap

module Rule :
  Generic.Proofrule.S
    with type seq_t = Program.Seq.t
     and type proof_t = Generic.Proof.Make(Program.Seq).t

module Seqtactics : Generic.Seqtactics.S with type seq_t = Program.Seq.t

module Proof :
  Generic.Proof.S
    with type t = Generic.Proof.Make(Program.Seq).t
     and type seq_t = Program.Seq.t
     and type node_t = Generic.Proofnode.Make(Program.Seq).t

module Slprover :
  Generic.Prover.S
    with type Seq.t = Seplog.Seq.t
     and type rule_t = Generic.Proofrule.Make(Seplog.Seq).t
     and module Proof = Generic.Proof.Make(Seplog.Seq)

module EntlSeqHash :
  Hashtbl.S
    with type key = Seplog.Seq.t
     and type 'a t = 'a Hashtbl.Make(Seplog.Seq).t

module ProofNode :
  Generic.Proofnode.S
    with type t = Generic.Proofnode.Make(Program.Seq).t
     and type seq_t = Program.Seq.t

val check_invalid : (EntlSeqHash.key -> bool) ref
val show_invalidity_debug : bool ref
val show_entailment_debug : bool ref
val show_frame_debug : bool ref
val entl_depth : int ref
val entailment_table : (int * Slprover.Proof.t option) EntlSeqHash.t
val entails : Seplog.Form.t -> Seplog.Form.t -> Slprover.Proof.t option

module AbdTblElt : sig
  include Lib.BasicType with type t = Generic.Tags.t * Seplog.Term.Set.t

  include
    Lib.Containers.S
      with type Set.elt = Generic.Tags.t * Seplog.Term.Set.t
       and type Map.key = Generic.Tags.t * Seplog.Term.Set.t
       and type Hashmap.key = Generic.Tags.t * Seplog.Term.Set.t
       and type Hashset.elt = Generic.Tags.t * Seplog.Term.Set.t
       and type MSet.elt = Generic.Tags.t * Seplog.Term.Set.t
       and type FList.t = (Generic.Tags.t * Seplog.Term.Set.t) list
end

module AbdTblMap = AbdTblElt.Hashmap

val abd_pre_table :
  (int
  * ((Seplog.Form.t * Seplog.Form.t) * Seplog.Unify.Unidirectional.state list)
    option)
  AbdTblMap.t
  EntlSeqHash.t

val abd_pre_transforms :
  Generic.Tags.t * Seplog.Term.Set.t ->
  Seplog.Form.t ->
  Seplog.Form.t ->
  ((Seplog.Form.t * Seplog.Form.t) * Seplog.Unify.Unidirectional.state list)
  option

val tagpairs : Seplog.Form.t * 'a * 'b -> Generic.Tagpairs.t
val progpairs : Generic.Tagpairs.t -> Generic.Tagpairs.t

val dest_sh_seq :
  Seplog.Form.t * 'a * 'b ->
  (Generic.Ord_constraints.t * Seplog.Heap.t) * 'a * 'b

val ex_falso_axiom : Rule.t
val mk_symex_empty_axiom : Rule.t

val eq_subst_ex_f :
  Seplog.Form.t * 'a * 'b ->
  (((Seplog.Form.t * 'a * 'b) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list

val simplify_rules :
  (Seplog.Form.t * 'a * 'b ->
  (((Seplog.Form.t * 'a * 'b) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * string)
  list)
  list

val simplify_seq_rl : Seqtactics.t
val simplify : Rule.t
val wrap : Seqtactics.t -> Rule.t
val lab_ex_intro : Rule.t
val lhs_disj_to_symheaps : Rule.t

val luf_rl :
  Seplog.Defs.t ->
  Seplog.Form.t * 'a * Seplog.Form.t ->
  ((((Generic.Ord_constraints.t * Seplog.Heap.t list) * 'a * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val luf : Seplog.Defs.t -> Rule.t

val ruf_rl :
  Seplog.Defs.t ->
  Seplog.Form.t * 'a * Seplog.Form.t ->
  (((Seplog.Form.t * 'a * (Generic.Ord_constraints.t * Seplog.Heap.t list))
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val ruf : Seplog.Defs.t -> Rule.t

val fix_tps :
  ((Seplog.Form.t * 'a * 'b) list * 'c) list ->
  (((Seplog.Form.t * 'a * 'b) * Generic.Tagpairs.t * Generic.Tagpairs.t) list
  * 'c)
  list

val mk_symex :
  (Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (Seplog.Heap.t list * string) list) ->
  Rule.t

val symex_assign_rule : Rule.t
val find_pto_on : Seplog.Heap.t -> Seplog.Term.t -> Seplog.Ptos.elt
val symex_load_rule : Rule.t
val symex_store_rule : Rule.t
val symex_free_rule : Rule.t
val symex_new_rule : Rule.t
val symex_skip_rule : Rule.t
val symex_if_rule : Rule.t
val symex_ifelse_rule : Rule.t
val symex_while_rule : Rule.t
val proc_unfold_str : string

val mk_symex_proc_unfold :
  (string
  * Seplog.Term.FList.t
  * (Seplog.Form.t * Seplog.Form.t) list
  * Program.Cmd.t)
  list ->
  'a Program.Proc.SigMap.t ref ->
  Rule.t

val is_proc_unfold_node : ProofNode.t -> bool
val assert_rule : Rule.t

val param_subst_rule :
  Seplog.Subst.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val subst_rule :
  Seplog.Subst.t * Generic.Tagpairs.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val left_or_elim_rule :
  ('a * Seplog.Heap.t list) * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((('a * Seplog.Heap.t list) * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val left_cut_rule :
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val right_cut_rule :
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val ex_intro_rule :
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val seq_rule :
  Seplog.Form.t ->
  Seplog.Form.t * 'a list * Seplog.Form.t ->
  (((Seplog.Form.t * 'a list * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val frame_rule :
  Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  (((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val schema_intro_rule :
  (Generic.Ord_constraints.t * Seplog.Heap.t list)
  * Program.Cmd.t
  * Seplog.Form.t ->
  (Generic.Ord_constraints.t * Seplog.Heap.t list)
  * Program.Cmd.t
  * Seplog.Form.t ->
  ((((Generic.Ord_constraints.t * Seplog.Heap.t list)
    * Program.Cmd.t
    * Seplog.Form.t)
   * Generic.Tagpairs.t
   * Generic.Tagpairs.t)
   list
  * string)
  list

val transform_seq :
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  ?match_post:bool ->
  Seplog.Form.t * Program.Cmd.t * Seplog.Form.t ->
  ((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t) * Rule.t) list

val mk_proc_call_rule_seq :
  ((Seplog.Form.t * Program.Cmd.t * Seplog.Form.t) * Seplog.Subst.t)
  * (Seplog.Form.t * Program.Cmd.t * Seplog.Form.t)
  * bool ->
  (Seplog.Form.t * Program.Cmd.t * Seplog.Form.t) * Rule.t ->
  Rule.t

val mk_symex_proc_call :
  Program.Proc.t list -> int -> Proof.t -> (int list * Proof.t) list

val dobackl :
  ?get_targets:Rule.select_f ->
  ?choose_all:bool ->
  int ->
  Proof.t ->
  (int list * Proof.t) list

val use_proc_prf :
  Proof.t option Program.Proc.SigMap.t ref ->
  int ->
  Proof.t ->
  (int list * Proof.t) list

val axioms : Rule.t ref
val rules : Rule.t ref

val setup :
  Seplog.Defs.t * Program.Proc.t list * Proof.t option Program.Proc.SigMap.t ref ->
  unit
