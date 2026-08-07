val termination : bool ref

module Field = While.Program.Field
module Cond = While.Program.Cond
module Cmd = While.Program.Cmd

exception WrongCmd

val main : string

module Proc : sig
  module K : sig
    type t =
      string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t

    val compare : string * 'a * 'b * 'c -> string * 'd * 'e * 'f -> int
    val equal : string * 'a * 'b * 'c -> string * 'd * 'e * 'f -> bool
    val hash : string * 'a * 'b * 'c -> int

    val pp_decl :
      Format.formatter -> string * Seplog.Term.t list * 'a * 'b -> unit

    val pp_specs :
      Format.formatter ->
      'a * 'b * (Seplog.Form.t * Seplog.Form.t) list * 'c ->
      unit

    val pp_head :
      Format.formatter ->
      string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * 'a ->
      unit

    val pp :
      Format.formatter ->
      string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t ->
      unit

    val to_string :
      string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t ->
      string
  end

  type t =
    string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t

  val compare : string * 'a * 'b * 'c -> string * 'd * 'e * 'f -> int
  val equal : string * 'a * 'b * 'c -> string * 'd * 'e * 'f -> bool
  val hash : string * 'a * 'b * 'c -> int

  val pp_decl :
    Format.formatter -> string * Seplog.Term.t list * 'a * 'b -> unit

  val pp_specs :
    Format.formatter ->
    'a * 'b * (Seplog.Form.t * Seplog.Form.t) list * 'c ->
    unit

  val pp_head :
    Format.formatter ->
    string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * 'a ->
    unit

  val pp :
    Format.formatter ->
    string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t ->
    unit

  val to_string :
    string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t ->
    string

  module Set : sig
    type t = Lib.Treeset.Make(K).t

    val hash : t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit

    type elt = K.t

    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val to_list : t -> elt list
    val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
    val opt_map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b option) -> t -> 'a
    val map_to_list : (elt -> 'a) -> t -> 'a list

    val weave :
      (elt -> 'a -> 'a list) ->
      (elt -> 'a -> 'b) ->
      ('b list -> 'b) ->
      t ->
      'a ->
      'b

    val find_suchthat : (elt -> bool) -> t -> elt
    val find_suchthat_opt : (elt -> bool) -> t -> elt option
    val find_map : (elt -> 'a option) -> t -> 'a option
    val count : (elt -> bool) -> t -> int
    val union_of_list : t list -> t
    val subsets : t -> t list
    val fixpoint : (t -> t) -> t -> t
    val del_first : (elt -> bool) -> t -> t
    val disjoint : t -> t -> bool

    val mk_unifier :
      bool ->
      bool ->
      ('a, 'b, elt) Lib.Unification.cps_unifier ->
      ('a, 'b, t) Lib.Unification.cps_unifier
  end

  module SigMap : sig
    type key =
      Lib.Pair.Make(Lib.Pair.Make(Lib.Strng)(Seplog.Term.FList))
        (Lib.Pair.Make(Seplog.Form)(Seplog.Form))
      .t

    type 'a t =
      'a
      Lib.Treemap.Make
        (Lib.Pair.Make(Lib.Pair.Make(Lib.Strng)(Seplog.Term.FList))
           (Lib.Pair.Make(Seplog.Form)(Seplog.Form)))
      .t

    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t

    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t

    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

    val to_string : ('a -> string) -> 'a t -> string
    val hash : ('a -> int) -> 'a t -> int
    val of_list : (key * 'a) list -> 'a t
    val to_list : 'a t -> (key * 'a) list
    val union : 'a t -> 'a t -> 'a t
    val find_map : (key -> 'a -> bool) -> 'a t -> (key * 'a) option
    val fixpoint : ('a -> 'a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
    val submap : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val add_bindings : (key * 'a) list -> 'a t -> 'a t
  end

  module Graph : sig
    type t = Graph.Imperative.Digraph.ConcreteBidirectional(K).t

    module V : sig
      type t = K.t

      val compare : t -> t -> int
      val hash : t -> int
      val equal : t -> t -> bool

      type label = t

      val create : label -> t
      val label : t -> label
    end

    type vertex = K.t

    module E : sig
      type t = vertex * vertex

      val compare : t -> t -> int

      type vertex = K.t

      val src : t -> vertex
      val dst : t -> vertex

      type label = unit

      val create : vertex -> label -> vertex -> t
      val label : t -> label
    end

    type edge = E.t

    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> edge list
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val create : ?size:int -> unit -> t
    val clear : t -> unit
    val copy : t -> t
    val add_vertex : t -> vertex -> unit
    val remove_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> unit
    val add_edge_e : t -> edge -> unit
    val remove_edge : t -> vertex -> vertex -> unit
    val remove_edge_e : t -> edge -> unit
  end

  val get_name : t -> string
  val get_params : t -> Seplog.Term.t list
  val get_spec_list : t -> (Seplog.Form.t * Seplog.Form.t) list
  val get_body : t -> Cmd.t
  val get_seqs : t -> (Seplog.Form.t * Cmd.basic_t list * Seplog.Form.t) list

  val number_cmds :
    t ->
    string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t

  val get_dependencies : t -> Lib.Strng.Set.t
  val parse_precondition : 'a MParser.state -> (Seplog.Form.t, 'a) MParser.reply

  val parse_postcondition :
    'a MParser.state -> (Seplog.Form.t, 'a) MParser.reply

  val ensure_tags :
    Seplog.Form.t * Seplog.Form.t -> Seplog.Form.t * Seplog.Form.t

  val check_spec :
    Seplog.Term.t list * Cmd.t -> Seplog.Form.t * Seplog.Form.t -> unit

  val parse_named :
    'a MParser.state ->
    ( string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t,
      'a )
    MParser.reply

  val parse_unnamed :
    'a MParser.state ->
    (Seplog.Form.t * Cmd.t * Seplog.Form.t, 'a) MParser.reply
end

module Seq : sig
  type t = Seplog.Form.t * Cmd.t * Seplog.Form.t

  val tagset_one : Generic.Tags.t
  val tagpairs_one : Generic.Tagpairs.t
  val form_tags : Seplog.Form.t -> Generic.Tags.t
  val tags : Seplog.Form.t * 'a * 'b -> Generic.Tags.t
  val all_tags : Seplog.Form.t * 'a * Seplog.Form.t -> Generic.Tags.t
  val tag_pairs : Seplog.Form.t * 'a * 'b -> Generic.Tagpairs.t
  val vars : Seplog.Form.t * 'a * Seplog.Form.t -> Seplog.Subst.var_container

  val all_vars :
    Seplog.Form.t * Cmd.t * Seplog.Form.t -> Seplog.Subst.var_container

  val terms : Seplog.Form.t * 'a * Seplog.Form.t -> Seplog.Subst.var_container

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

val program_vars : Seplog.Subst.var_container ref
val proc_list : Proc.Graph.vertex list ref
val proc_map : Proc.Graph.vertex Lib.Strng.Map.t ref

module Operations : sig
  type g = Graph.Imperative.Digraph.ConcreteBidirectional(Proc.K).t

  val transitive_closure : ?reflexive:bool -> g -> g
  val add_transitive_closure : ?reflexive:bool -> g -> g
  val transitive_reduction : ?reflexive:bool -> g -> g
  val replace_by_transitive_reduction : ?reflexive:bool -> g -> g
  val mirror : g -> g
  val complement : g -> g
  val intersect : g -> g -> g
  val union : g -> g -> g
end

val dependencies : Proc.Graph.t ref
val reachability : Proc.Graph.t ref
val pp : Format.formatter -> unit -> unit
val get_proc : string -> Proc.Graph.vertex
val set_program : string list * Proc.Graph.vertex list -> unit
val get_reachable : string list -> Proc.Graph.t
val vars_of_program : unit -> Seplog.Subst.var_container
val fresh_fvar : Seplog.Subst.var_container -> Seplog.Term.t
val fresh_fvars : Seplog.Subst.var_container -> int -> Seplog.Term.t list
val fresh_evar : Seplog.Subst.var_container -> Seplog.Term.t
val fresh_evars : Seplog.Subst.var_container -> int -> Seplog.Term.t list

val freshen_case_by_seq :
  Seplog.Form.t * 'a * Seplog.Form.t -> Seplog.Indrule.t -> Seplog.Indrule.t

val parse_fields : 'a MParser.state -> (string list, 'a) MParser.reply

val parse_procs :
  'a MParser.state ->
  ( (string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t)
    list,
    'a )
  MParser.reply

val parse :
  'a MParser.state ->
  ( string list
    * (string
      * Seplog.Term.t list
      * (Seplog.Form.t * Seplog.Form.t) list
      * Cmd.t)
      list,
    'a )
  MParser.reply

val of_channel :
  in_channel ->
  string list
  * (string * Seplog.Term.t list * (Seplog.Form.t * Seplog.Form.t) list * Cmd.t)
    list
