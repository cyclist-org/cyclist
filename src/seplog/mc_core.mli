module List = Lib.Blist

module type NaturalType = sig
  include Lib.BasicType

  val zero : t
  val succ : t -> t
end

module NatType : NaturalType with type t = int

module Var : sig
  include Lib.BasicType
  module Set : Lib.OrderedContainer with type elt = t
  module Map : Lib.OrderedMap with type key = t

  val of_term : Term.t -> t
  val to_term : t -> Term.t
  val parse : (t, 'a) MParser.t
end

module type ParserSig = sig
  type t

  val parse : (t, 'a) MParser.t
  val of_string : string -> t
end

module type S = sig
  module Location : NaturalType
  module Scalar : NaturalType

  module Value : sig
    include NaturalType

    val mk_loc_val : Location.t -> t
    val mk_scalar_val : Scalar.t -> t
  end

  module ConcreteHeap : sig
    include Lib.BasicType

    module MakeParser (T : sig
      val parse_scalar : (Value.t, 'a) MParser.t
      val parse_location : (Location.t, 'a) MParser.t
    end) : ParserSig with type t = t

    val size : t -> int
  end

  module Stack : sig
    include Lib.BasicType

    module MakeParser (T : sig
      val parse_scalar : (Value.t, 'a) MParser.t
    end) : ParserSig with type t = t
  end

  type model = Stack.t * ConcreteHeap.t

  val mk_model_parser :
    (Stack.t, 'a) MParser.t * (ConcreteHeap.t, 'a) MParser.t ->
    (model, 'a) MParser.t

  val model_of_string :
    (Stack.t * ConcreteHeap.t, unit) MParser.t -> string -> model

  val check_model : Defs.t -> Heap.t * model -> bool
end

module type ValueSig = sig
  module HeapLocation : NaturalType
  module ScalarValue : NaturalType

  val pp_nil : Format.formatter -> unit
end

module Make (Sig : ValueSig) : sig
  val max_hashset_size : int ref

  module Location : sig
    include NaturalType with type t = Sig.HeapLocation.t

    include
      Lib.Containers.S
        with type Set.elt = Sig.HeapLocation.t
         and type Map.key = Sig.HeapLocation.t
         and type Hashmap.key = Sig.HeapLocation.t
         and type Hashset.elt = Sig.HeapLocation.t
         and type MSet.elt = Sig.HeapLocation.t
         and type FList.t = Sig.HeapLocation.t list
  end

  module Scalar : NaturalType with type t = Sig.ScalarValue.t

  module Value : sig
    module T : sig
      type t = Nil | Location of Location.t | Scalar of Scalar.t

      include NaturalType with type t := t
    end

    type t = T.t = Nil | Location of Location.t | Scalar of Scalar.t

    include NaturalType with type t := t

    include
      Lib.Containers.S
        with type Set.elt = T.t
         and type Map.key = T.t
         and type Hashmap.key = T.t
         and type Hashset.elt = T.t
         and type MSet.elt = T.t
         and type FList.t = T.t list

    val mk_loc_val : Location.t -> t
    val mk_scalar_val : Scalar.t -> t
    val nil : t
  end

  module ConcreteHeap : sig
    type t = Value.FList.t Location.Map.t
    type domain = Location.Set.t

    include Lib.BasicType with type t := t

    val get_all_vals : t -> Value.Set.t
    val size : t -> int

    type heap = t

    module MakeParser (T : sig
      val parse_scalar : (Value.t, 'a) MParser.t
      val parse_location : (Location.t, 'a) MParser.t
    end) : ParserSig with type t = heap
  end

  module Stack : sig
    type t = Value.t Var.Map.t

    include Lib.BasicType with type t := t

    val vars : t -> Var.Set.t
    val get_all_vals : t -> Value.Set.t
    val empty : t
    val of_term_bindings : (Term.t * Value.t) list -> t option
    val consistent : t -> t -> bool
    val merge : t -> t -> t
    val satisfies : Uf.t * Deqs.t -> t -> bool
    val cross_satisfies : Uf.t * Deqs.t -> t -> t -> bool

    type stack = t

    module MakeParser (T : sig
      val parse_scalar : (Value.t, 'a) MParser.t
    end) : ParserSig with type t = stack
  end

  val mk_model_parser :
    ('a, 'b) MParser.t * ('c, 'b) MParser.t -> ('a * 'c, 'b) MParser.t

  val model_of_string : ('a, unit) MParser.t -> string -> 'a

  module SetBase : sig
    include
      Lib.OrderedContainer
        with type t = ConcreteHeap.domain
         and type elt = Location.t

    val inj : ConcreteHeap.t -> ConcreteHeap.t -> t
    val proj : ConcreteHeap.t -> t -> ConcreteHeap.t
  end

  module HeapBase : sig
    include Lib.BasicType

    val empty : t
    val inj : ConcreteHeap.t -> ConcreteHeap.t -> t
    val proj : ConcreteHeap.t -> t -> ConcreteHeap.t
    val disjoint : t -> t -> bool
    val union : t -> t -> t
  end

  module InterpretantBaseContainers :
    Lib.Containers.S
      with type Set.elt = Value.FList.t * HeapBase.t
       and type Map.key = Value.FList.t * HeapBase.t
       and type Hashmap.key = Value.FList.t * HeapBase.t
       and type Hashset.elt = Value.FList.t * HeapBase.t
       and type MSet.elt = Value.FList.t * HeapBase.t
       and type FList.t = (Value.FList.t * HeapBase.t) list

  module InterpretantBase = InterpretantBaseContainers.Hashset

  val baseSetPair_to_string : InterpretantBase.t * InterpretantBase.t -> string

  module SymHeapHash :
    Hashtbl.S with type key = Heap.t and type 'a t = 'a Hashtbl.Make(Heap).t

  module SymHeapHashPrinter : sig
    val pp :
      (Format.formatter -> SymHeapHash.key -> unit) ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a SymHeapHash.t ->
      unit

    val to_string :
      (Format.formatter -> SymHeapHash.key -> unit) ->
      (Format.formatter -> 'a -> unit) ->
      'a SymHeapHash.t ->
      string
  end

  module ModelBase :
    Lib.Containers.S
      with type Set.elt = Stack.t * HeapBase.t
       and type Map.key = Stack.t * HeapBase.t
       and type Hashmap.key = Stack.t * HeapBase.t
       and type Hashset.elt = Stack.t * HeapBase.t
       and type MSet.elt = Stack.t * HeapBase.t
       and type FList.t = (Stack.t * HeapBase.t) list

  val empty_base : unit -> InterpretantBase.t

  val itp_emp : unit -> ModelBase.Hashset.t
  (** [itp_emp] is the minimal set of model bases of the formula emp, i.e. the
      singleton set containing the model base consisting of the empty stack and
      the empty heap base. *)

  val init_empty :
    Defs.t -> (InterpretantBase.t * InterpretantBase.t) Predsym.Map.t

  val decorate :
    'a ->
    (InterpretantBase.t * InterpretantBase.t) Predsym.Map.t ->
    InterpretantBase.t Predsym.Map.t

  val add_spares : int -> Value.Set.t -> Value.Set.t

  val generate_model :
    Term.t list ->
    Uf.t * Deqs.t ->
    Value.t list * 'a ->
    (Value.t Var.Map.t * 'a) option
  (** Given a list of terms [ts] which are the formal parameters of some atomic
      spatial formula (predicate or points-to) F, some pure [constraints] Pi,
      and a set of interpretants [itpts] of F,
      [generate_models ts constraints itpts] generates a hashset of model bases
      which represents the interpretation of (Pi : F) **)

  val generate_models_ls :
    Term.t list ->
    Uf.t * Deqs.t ->
    (Value.t list * 'a) list ->
    (Value.t Var.Map.t * 'a) list

  val generate_models :
    Term.t list -> Uf.t * Deqs.t -> InterpretantBase.t -> ModelBase.Hashset.t

  val cross_model :
    Uf.t * Deqs.t ->
    Value.t Var.Map.t * HeapBase.t ->
    Value.t Var.Map.t * HeapBase.t ->
    (Value.t Var.Map.t * HeapBase.t) option
  (** Given some pure [constraints] Pi and two sets of model bases [ms] and
      [ms'] representing the interpretation of two formulas (Pi : F) and (Pi :
      G) respectively, [cross_models constraints ms ms'] generates the set of
      model bases that denotes the intepretation of (Pi : F * G). **)

  val cross_models_ls :
    Uf.t * Deqs.t ->
    (Value.t Var.Map.t * HeapBase.t) list ->
    (Value.t Var.Map.t * HeapBase.t) list ->
    (Value.t Var.Map.t * HeapBase.t) list

  val cross_models :
    Uf.t * Deqs.t ->
    ModelBase.Hashset.t ->
    ModelBase.Hashset.t ->
    ModelBase.Hashset.t

  val valid_extns :
    Uf.t * Deqs.t ->
    Value.Set.t ->
    Var.Set.t ->
    Value.t Var.Map.t ->
    ((Value.t list -> Value.t Var.Map.t option) -> Value.t list list -> 'a) ->
    'a

  val saturate_univs_one :
    Var.Set.t ->
    Uf.t * Deqs.t ->
    Value.Set.t ->
    Value.t Var.Map.t * 'a ->
    (Value.t Var.Map.t * 'a) list
  (** [saturate params constraints vs mdls] generates a new set of model bases
      from [mdls] by extending the stacks of each model base in [mdls] with
      mappings to values in [vs] from every universal variable either in params
      or mentioned in [constraints] that is not already mapped. Each model base
      in [mdls] gives rise to a new model base for every possible satisfying
      extension. Thus, every model base in [mdls] may give rise to zero or more
      models in the returned set. **)

  val saturate_univs_ls :
    Var.Set.t ->
    Uf.t * Deqs.t ->
    Value.Set.t ->
    (Value.t Var.Map.t * 'a) list ->
    (Value.t Var.Map.t * 'a) list

  val exs_satisfiable :
    Uf.t * Deqs.t -> Value.Set.t -> Value.t Var.Map.t -> bool
  (** [ex_constraint_sat constraints vs s] returns true if and only if the stack
      [s] can be extended with mappings from existential variables to values in
      [vs] such that that the extended stack has a mapping for every existential
      variable mentioned in [constraints] and also satisfies [constraints]. **)

  val mk_ptos_base :
    Defs.t -> ConcreteHeap.t -> ModelBase.Set.elt list SymHeapHash.t
  (** [mk_ptos_base defs h] creates a hashtable which stores a hashset of model
      bases for each inductive rule in [defs] that is both consistent and
      contains some number (> 0) of points-to formula atoms. These models are
      the valid interpretations of the entire set of points-to atoms in each
      inductive rule body whose heap is a subheap of [h]. The hastable is keyed
      on a symbolic heap formula. i.e. We abstract the points-to set for each
      rule and compute its interpretation only once before starting the fixpoint
      computation, and make it quickly accessible using a hash table.

      Notes: 1. [all_ptos_itpts] is a map containing all the interpretant bases
      of the singleton subheaps of [h] keyed on size of the heap cell being
      pointed to. This allows easy identification of only those subheaps
      relevant to any given points-to formula atom. 2. We calculate more or less
      the precise size we will need for the hashtable in [num_buckets]; this is
      done by counting the number of inductive rule bodies that are both
      consistent and have a greater than zero number of points-to formula atoms.
      Note that this is, in practice, a precise bound since it is unlikely that
      there will be exactly duplicated inductive rule bodies. **)

  val saturate_ls :
    Value.Set.t ->
    Term.t list ->
    Uf.t * Deqs.t ->
    (Stack.t * HeapBase.t) list ->
    (Stack.t * HeapBase.t) list

  val add_itpts_of_models_ls :
    Term.t list ->
    InterpretantBase.t ->
    (Value.t Var.Map.t * HeapBase.t) list ->
    unit

  val rule_gen :
    ModelBase.FList.t SymHeapHash.t ->
    Value.Set.t ->
    (InterpretantBase.t * InterpretantBase.t) Predsym.Map.t ->
    InterpretantBase.t Predsym.Map.t ->
    Indrule.t ->
    unit

  val mk_generator :
    Defs.t * (Value.t list * Value.FList.t Location.Map.t) ->
    unit ->
    (InterpretantBase.t * InterpretantBase.t) Predsym.Map.t

  val mk :
    Defs.t * (Value.t list * Value.FList.t Location.Map.t) ->
    InterpretantBase.t Predsym.Map.t

  val check_model :
    bool ->
    Defs.t ->
    SymHeapHash.key * (Value.t Var.Map.t * Value.t list Location.Map.t) ->
    bool
end

module IntSig :
  ValueSig
    with type HeapLocation.t = NatType.t
     and type ScalarValue.t = NatType.t
