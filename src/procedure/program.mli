val termination : bool ref

module Field = While.Program.Field
module Cond = While.Program.Cond
module Cmd = While.Program.Cmd

exception WrongCmd

val main : string

module Proc : sig
  (** The BasicType kernel (N.B. Procs are equal if they have the same name) *)
  module K : sig
    type t =
      string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t
    (** The type of procedures: a tuple of
        - Procedure name
        - Formal parameters
        - A list of pre/post specifications
        - The body of the procedure *)

    include Lib.BasicType with type t := t

    val pp_decl : Format.formatter -> t -> unit
    val pp_specs : Format.formatter -> t -> unit
    val pp_head : Format.formatter -> t -> unit
  end

  include module type of K
  module Set : Lib.OrderedContainer with type elt = K.t

  module SigMap :
    Lib.OrderedMap
      with type key =
        (string * Seplog.Term.FList.t) * (Seplog.Form.t * Seplog.Form.t)

  module Graph :
    Graph.Sig.I
      with type V.t = K.t
       and type V.label = K.t
       and type E.t = K.t * K.t
       and type E.label = unit

  val get_name : t -> string
  val get_params : t -> Seplog.Term.t list
  val get_spec_list : t -> (Seplog.Form.t * Seplog.Form.t) list
  val get_body : t -> Cmd.t
  val get_seqs : t -> (Seplog.Form.t * Cmd.basic_t list * Seplog.Form.t) list

  val number_cmds :
    t ->
    string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t

  val get_dependencies : t -> Lib.Strng.Set.t
  val parse_precondition : (Seplog.Form.t, 'a) MParser.t
  val parse_postcondition : (Seplog.Form.t, 'a) MParser.t

  val ensure_tags :
    Seplog.Form.t * Seplog.Form.t -> Seplog.Form.t * Seplog.Form.t

  val check_spec :
    Seplog.Term.t list * Cmd.t -> Seplog.Form.t * Seplog.Form.t -> unit

  val parse_named :
    ( string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t,
      'a )
    MParser.t

  val parse_unnamed : (Seplog.Form.t * Cmd.t * Seplog.Form.t, 'a) MParser.t
end

module Seq : sig
  type t = Seplog.Form.t * Cmd.t * Seplog.Form.t

  val tagset_one : Generic.Tags.t
  val tagpairs_one : Generic.Tagpairs.t
  val form_tags : Seplog.Form.t -> Generic.Tags.t
  val tags : Seplog.Form.t * 'a * 'b -> Generic.Tags.t
  val all_tags : Seplog.Form.t * 'a * Seplog.Form.t -> Generic.Tags.t
  val tag_pairs : Seplog.Form.t * 'a * 'b -> Generic.Tagpairs.t
  val vars : Seplog.Form.t * 'a * Seplog.Form.t -> Seplog.Term.Set.t
  val all_vars : Seplog.Form.t * Cmd.t * Seplog.Form.t -> Seplog.Term.Set.t
  val terms : Seplog.Form.t * 'a * Seplog.Form.t -> Seplog.Term.Set.t

  val subst :
    Seplog.Subst.t ->
    Seplog.Form.t * 'a * Seplog.Form.t ->
    Seplog.Form.t * 'a * Seplog.Form.t

  val subst_tags :
    Generic.Tagpairs.t ->
    Seplog.Form.t * 'a * Seplog.Form.t ->
    Seplog.Form.t * 'a * Seplog.Form.t

  val param_subst :
    Seplog.Subst.t ->
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    Seplog.Form.t * Cmd.t * Seplog.Form.t

  val with_pre : 'a * 'b * 'c -> 'd -> 'd * 'b * 'c
  val with_post : 'a * 'b * 'c -> 'd -> 'a * 'b * 'd
  val with_cmd : 'a * 'b * 'c -> 'd -> 'a * 'd * 'c
  val to_string : Seplog.Form.t * Cmd.t * Seplog.Form.t -> string

  val subsumed :
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    bool

  val subsumed_upto_tags :
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    bool

  val pp : Format.formatter -> Seplog.Form.t * Cmd.t * Seplog.Form.t -> unit

  val equal :
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    bool

  val equal_upto_tags :
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    Seplog.Form.t * Cmd.t * Seplog.Form.t ->
    bool

  val dest :
    Seplog.Form.t * 'a * Seplog.Form.t ->
    (Generic.Ord_constraints.t * Seplog.Heap.symheap)
    * 'a
    * (Generic.Ord_constraints.t * Seplog.Heap.symheap)

  val get_tracepairs :
    Seplog.Form.t * 'a * 'b ->
    Seplog.Form.t * 'c * 'd ->
    Generic.Tagpairs.t * Generic.Tagpairs.t

  val frame :
    Seplog.Form.t ->
    Seplog.Form.t * 'a * Seplog.Form.t ->
    Seplog.Form.t * 'a * Seplog.Form.t
end

val pp_prog :
  Format.formatter ->
  Lib.Strng.FList.t
  * (string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t)
    list ->
  unit

val program_vars : Seplog.Term.Set.t ref
val proc_list : Proc.Graph.vertex list ref
val proc_map : Proc.Graph.vertex Lib.Strng.Map.t ref

module Operations : Graph.Oper.S with type g = Proc.Graph.t

val dependencies : Proc.Graph.t ref
val reachability : Proc.Graph.t ref
val pp : Format.formatter -> unit -> unit
val get_proc : string -> Proc.Graph.vertex
val set_program : string list * Proc.Graph.vertex list -> unit
val get_reachable : string list -> Proc.Graph.t
val vars_of_program : unit -> Seplog.Term.Set.t
val fresh_fvar : Seplog.Term.Set.t -> Seplog.Term.t
val fresh_fvars : Seplog.Term.Set.t -> int -> Seplog.Term.t list
val fresh_evar : Seplog.Term.Set.t -> Seplog.Term.t
val fresh_evars : Seplog.Term.Set.t -> int -> Seplog.Term.t list

val freshen_case_by_seq :
  Seplog.Form.t * 'a * Seplog.Form.t -> Seplog.Indrule.t -> Seplog.Indrule.t

val parse_fields : (string list, 'a) MParser.t

val parse_procs :
  ( (string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t)
    list,
    'a )
  MParser.t

val parse :
  ( string list
    * (string
      * Seplog.Term.t list
      * (Seplog.Form.t * Seplog.Form.t) list
      * Cmd.t)
      list,
    'a )
  MParser.t

val of_channel :
  in_channel ->
  string list
  * (string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t)
    list
