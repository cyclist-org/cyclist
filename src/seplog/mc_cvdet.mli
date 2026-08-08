(** What a concrete-value deterministic model checker exposes. *)
module type S = sig
  module Location : sig
    include Lib.BasicType
    module Map : Lib.OrderedMap with type key = t
  end

  module Value : Lib.BasicType
  module Var = Mc_core.Var
  module Stack : Lib.BasicType
  module ConcreteHeap : Lib.BasicType with type t = Value.t list Location.Map.t

  module Reduction : sig
    type t = {
      stack : Stack.t;
      heap : ConcreteHeap.t option ref;
      symheap : Heap.t;
      remainder : ConcreteHeap.t option ref;
    }

    include Generic.Sequent.S with type t := t

    val dest :
      t -> Stack.t * ConcreteHeap.t * Heap.t * ConcreteHeap.t option ref

    val mk : Stack.t * ConcreteHeap.t * Heap.t * ConcreteHeap.t option -> t
  end

  module Rule :
    Generic.Proofrule.S
      with type seq_t = Reduction.t
       and type proof_t = Generic.Proof.Make(Reduction).t

  val set_metavar : 'a option ref -> 'a -> unit
  val emp_axiom : Rule.t
  val interpret : Value.t Var.Map.t -> Term.t -> Value.t
  val is_location : Value.t -> bool
  val get_location : Value.t -> Location.t
  val all_vars_free : Heap.t -> bool
  val points_to_axiom : Rule.t
  val mk_infrule : (Rule.seq_t -> (Rule.seq_t list * string) list) -> Rule.t
  val discharge_eq : Rule.t
  val discharge_deq : Rule.t
  val discharge_pto : Rule.t
  val discharge_ind : Rule.t
  val eliminate_eq : Rule.t
  val eliminate_pto : Rule.t
  val defs : Defs.t ref
  val select_rule : Value.t list -> Indrule.t list -> Indrule.t option
  val unfold : Rule.t
  val axioms : Rule.t
  val rules : Rule.t
end

module Make (Sig : Mc_core.ValueSig) :
  S
    with type Location.t = Mc_core.Make(Sig).Location.t
     and type 'a Location.Map.t = 'a Mc_core.Make(Sig).Location.Map.t
     and type Value.t = Mc_core.Make(Sig).Value.t
     and type Stack.t = Mc_core.Make(Sig).Stack.t

module IntSigModelChecker :
  S
    with type Location.t = Mc_core.Make(Mc_core.IntSig).Location.t
     and type 'a Location.Map.t = 'a Mc_core.Make(Mc_core.IntSig).Location.Map.t
     and type Value.t = Mc_core.Make(Mc_core.IntSig).Value.t
     and type Stack.t = Mc_core.Make(Mc_core.IntSig).Stack.t

module Prover :
  Generic.Prover.S
    with type Seq.t = IntSigModelChecker.Reduction.t
     and type rule_t = Generic.Proofrule.Make(IntSigModelChecker.Reduction).t
     and module Proof = Generic.Proof.Make(IntSigModelChecker.Reduction)

val check_model :
  bool ->
  Defs.t ->
  Heap.t * (IntSigModelChecker.Stack.t * IntSigModelChecker.ConcreteHeap.t) ->
  bool
