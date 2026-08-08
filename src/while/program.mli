module SH = Seplog.Heap

val termination : bool ref

module Field : sig
  type t = string

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val add : t -> unit
  val get_index : t -> int
  val get_fields : unit -> t list
  val get_no_fields : unit -> int
  val pp_fields : Format.formatter -> Lib.Strng.FList.t -> unit
  val pp : Format.formatter -> unit -> unit
  val reset : unit -> unit
  val parse : (t, 'a) MParser.t
end

exception WrongCmd

val is_prog_var : Seplog.Term.t -> bool
val is_prog_term : Seplog.Term.t -> bool

module Cond : sig
  type t =
    | Eq of Seplog.Term.t * Seplog.Term.t
    | Deq of Seplog.Term.t * Seplog.Term.t
    | Non_det

  val mk_eq : Seplog.Term.t -> Seplog.Term.t -> t
  val mk_deq : Seplog.Term.t -> Seplog.Term.t -> t
  val mk_non_det : unit -> t
  val is_deq : t -> bool
  val is_eq : t -> bool
  val is_non_det : t -> bool
  val is_det : t -> bool
  val dest : t -> Seplog.Term.t * Seplog.Term.t
  val terms : t -> Seplog.Term.Set.t
  val vars : t -> Seplog.Term.Set.t
  val equal : t -> t -> bool
  val subst : Seplog.Subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val fork : SH.t -> t -> SH.t * SH.t
  val validated_by : SH.t -> t -> bool
  val invalidated_by : SH.t -> t -> bool
  val parse : (t, 'a) MParser.t
end

module Cmd : sig
  type cmd_t =
    | Stop
    | Return
    | Skip
    | Assign of Seplog.Term.t * Seplog.Term.t
    | Load of Seplog.Term.t * Seplog.Term.t * string
    | Store of Seplog.Term.t * string * Seplog.Term.t
    | New of Seplog.Term.t
    | Free of Seplog.Term.t
    | If of Cond.t * t
    | IfElse of Cond.t * t * t
    | While of Cond.t * t
    | ProcCall of string * Seplog.Term.FList.t
    | Assert of Seplog.Form.t

  and basic_t = { label : int option; cmd : cmd_t }
  and t = basic_t list

  val get_cmd : basic_t list -> cmd_t
  val get_cont : 'a list -> 'a list
  val split : 'a list -> 'a list * 'a list
  val is_empty : 'a list -> bool
  val is_not_empty : 'a list -> bool
  val is_assign : basic_t list -> bool
  val is_load : basic_t list -> bool
  val is_store : basic_t list -> bool
  val is_new : basic_t list -> bool
  val is_free : basic_t list -> bool
  val is_stop : basic_t list -> bool
  val is_return : basic_t list -> bool
  val is_skip : basic_t list -> bool
  val is_proc_call : basic_t list -> bool
  val is_assert : basic_t list -> bool
  val is_basic : basic_t list -> bool
  val is_final : basic_t list -> bool
  val is_if : basic_t list -> bool
  val is_ifelse : basic_t list -> bool
  val is_while : basic_t list -> bool
  val mklc : cmd_t -> basic_t
  val mk_basic : cmd_t -> basic_t list
  val mk_assign : Seplog.Term.t -> Seplog.Term.t -> basic_t list
  val mk_load : Seplog.Term.t -> Seplog.Term.t -> string -> basic_t list
  val mk_store : Seplog.Term.t -> string -> Seplog.Term.t -> basic_t list
  val mk_new : Seplog.Term.t -> basic_t list
  val mk_free : Seplog.Term.t -> basic_t list
  val mk_stop : basic_t list
  val mk_return : basic_t list
  val mk_skip : basic_t list
  val mk_proc_call : string -> Seplog.Term.FList.t -> basic_t list
  val mk_assert : Seplog.Form.t -> basic_t list
  val mk_if : Cond.t -> t -> basic_t list
  val mk_ifelse : Cond.t -> t -> t -> basic_t list
  val mk_while : Cond.t -> t -> basic_t list
  val mk_seq : 'a list -> 'a list -> 'a list
  val mk_from_list : 'a list list -> 'a list
  val parse_cmd_opt : (cmd_t option, 'a) MParser.t
  val parse : (t, 'a) MParser.t
  val dest_cmd : (cmd_t -> 'a) -> basic_t list -> 'a
  val dest_stop : basic_t list -> unit
  val dest_return : basic_t list -> unit
  val dest_skip : basic_t list -> unit
  val dest_assign : basic_t list -> Seplog.Term.t * Seplog.Term.t
  val dest_load : basic_t list -> Seplog.Term.t * Seplog.Term.t * string
  val dest_store : basic_t list -> Seplog.Term.t * string * Seplog.Term.t
  val dest_new : basic_t list -> Seplog.Term.t
  val dest_free : basic_t list -> Seplog.Term.t
  val dest_deref : basic_t list -> Seplog.Term.t
  val dest_if : basic_t list -> Cond.t * t
  val dest_ifelse : basic_t list -> Cond.t * t * t
  val dest_while : basic_t list -> Cond.t * t
  val dest_branching : basic_t list -> Cond.t
  val dest_proc_call : basic_t list -> string * Seplog.Term.FList.t
  val dest_assert : basic_t list -> Seplog.Form.t
  val dest_empty : 'a list -> unit
  val number : t -> t
  val cmd_terms : cmd_t -> Seplog.Term.Set.t
  val terms : t -> Seplog.Term.Set.t
  val vars : t -> Seplog.Term.Set.t
  val locals : Seplog.Term.Set.t -> t -> Seplog.Term.Set.t
  val cmd_modifies : ?strict:bool -> cmd_t -> Seplog.Term.Set.t
  val modifies : ?strict:bool -> t -> Seplog.Term.Set.t
  val cmd_equal : cmd_t -> cmd_t -> bool
  val equal : t -> t -> bool
  val subst_cmd : Seplog.Subst.t -> cmd_t -> cmd_t
  val subst : Seplog.Subst.t -> t -> t
  val number_width : int ref
  val indent_by : int ref
  val pp_label : ?abbr:bool -> int -> Format.formatter -> basic_t -> unit
  val pp_cmd : ?abbr:bool -> int -> Format.formatter -> basic_t -> unit
  val pp_lcmd : ?abbr:bool -> int -> Format.formatter -> basic_t -> unit
  val pp : ?abbr:bool -> int -> Format.formatter -> t -> unit
  val to_string : t -> string
  val strip_asserts : basic_t list -> basic_t list
  val is_while_prog : t -> bool
  val get_dependencies : t -> Lib.Strng.Set.t
end

val program_pp : Format.formatter -> Cmd.t -> unit
val pp_cmd : Format.formatter -> Cmd.t -> unit

module Seq : sig
  type t = Seplog.Form.t * Cmd.t

  val tagset_one : Generic.Tags.t
  val tagpairs_one : Generic.Tagpairs.t
  val tags : Seplog.Form.t * 'a -> Generic.Tags.t
  val tag_pairs : Seplog.Form.t * 'a -> Generic.Tagpairs.t
  val vars : Seplog.Form.t * 'a -> Seplog.Term.Set.t
  val terms : Seplog.Form.t * 'a -> Seplog.Term.Set.t
  val subst : Seplog.Subst.t -> Seplog.Form.t * 'a -> Seplog.Form.t * 'a
  val to_string : Seplog.Form.t * Cmd.t -> string
  val pp : Format.formatter -> Seplog.Form.t * Cmd.t -> unit
  val equal : Seplog.Form.t * Cmd.t -> Seplog.Form.t * Cmd.t -> bool
  val equal_upto_tags : Seplog.Form.t * Cmd.t -> Seplog.Form.t * Cmd.t -> bool
  val subsumed : Seplog.Form.t * Cmd.t -> Seplog.Form.t * Cmd.t -> bool

  val subsumed_upto_tags :
    Seplog.Form.t * Cmd.t -> Seplog.Form.t * Cmd.t -> bool

  val subst_tags :
    Generic.Tagpairs.t -> Seplog.Form.t * 'a -> Seplog.Form.t * 'a
end

val program_vars : Seplog.Term.Set.t ref
val set_program : Cmd.t -> unit
val vars_of_program : unit -> Seplog.Term.Set.t
val fresh_fvar : Seplog.Term.Set.t -> Seplog.Term.t
val fresh_fvars : Seplog.Term.Set.t -> int -> Seplog.Term.t list
val fresh_evar : Seplog.Term.Set.t -> Seplog.Term.t
val fresh_evars : Seplog.Term.Set.t -> int -> Seplog.Term.t list

val freshen_case_by_seq :
  Seplog.Form.t * 'a -> Seplog.Indrule.t -> Seplog.Indrule.t

val parse_fields : (unit, 'a) MParser.t
val parse_precondition : ?allow_tags:bool -> (Seplog.Form.t, 'a) MParser.t
val parse : (Seplog.Form.t * Cmd.t, 'a) MParser.t
val of_channel : in_channel -> Seplog.Form.t * Cmd.t
