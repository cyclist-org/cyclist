module SH = Asl.Asl_heap

exception WrongCmd

val is_prog_var : Asl.Asl_term.term_t -> bool
val is_prog_term : Asl.Asl_term.term_t -> bool

module Cond : sig
  type t =
    | Eq of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Ne of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Lt of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Le of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Non_det

  val mk_eq : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> t
  val mk_ne : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> t
  val mk_lt : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> t
  val mk_le : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> t
  val mk_non_det : unit -> t
  val is_ne : t -> bool
  val is_eq : t -> bool
  val is_le : t -> bool
  val is_lt : t -> bool
  val is_non_det : t -> bool
  val is_det : t -> bool
  val dest : t -> Asl.Asl_term.term_t * Asl.Asl_term.term_t
  val terms : t -> Asl.Asl_term.Set.t
  val vars : t -> Asl.Asl_term.Set.t
  val equal : t -> t -> bool
  val subst : Asl.Asl_term.Asl_subst.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val fork : SH.t -> t -> SH.t * SH.t
  val validated_by : SH.t -> t -> bool
  val invalidated_by : SH.t -> t -> bool
  val parse : (t, 'a) MParser.t
end

module Cmd : sig
  type cmd_t =
    | Stop
    | Skip
    | Assign of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Load of Asl.Asl_term.term_t * Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Store of Asl.Asl_term.term_t * Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | New of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | Free of Asl.Asl_term.term_t * Asl.Asl_term.term_t
    | If of Cond.t * t
    | IfElse of Cond.t * t * t
    | While of Cond.t * t

  and basic_t = { label : int option; cmd : cmd_t }
  and t = basic_t list

  val get_cmd : basic_t list -> cmd_t
  val get_cont : 'a list -> 'a list
  val is_empty : 'a list -> bool
  val is_not_empty : 'a list -> bool
  val is_assign : basic_t list -> bool
  val is_load : basic_t list -> bool
  val is_store : basic_t list -> bool
  val is_new : basic_t list -> bool
  val is_free : basic_t list -> bool
  val is_stop : basic_t list -> bool
  val is_skip : basic_t list -> bool
  val is_basic : basic_t list -> bool
  val is_if : basic_t list -> bool
  val is_ifelse : basic_t list -> bool
  val is_while : basic_t list -> bool
  val mklc : cmd_t -> basic_t
  val mk_basic : cmd_t -> basic_t list
  val mk_assign : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> basic_t list

  val mk_load :
    Asl.Asl_term.term_t ->
    Asl.Asl_term.term_t ->
    Asl.Asl_term.term_t ->
    basic_t list

  val mk_store :
    Asl.Asl_term.term_t ->
    Asl.Asl_term.term_t ->
    Asl.Asl_term.term_t ->
    basic_t list

  val mk_new : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> basic_t list
  val mk_free : Asl.Asl_term.term_t -> Asl.Asl_term.term_t -> basic_t list
  val mk_stop : basic_t list
  val mk_skip : basic_t list
  val mk_if : Cond.t -> t -> basic_t list
  val mk_ifelse : Cond.t -> t -> t -> basic_t list
  val mk_while : Cond.t -> t -> basic_t list
  val mk_seq : 'a list -> 'a list -> 'a list
  val mk_from_list : 'a list list -> 'a list
  val parse_cmd : (cmd_t, 'a) MParser.t
  val parse : (t, 'a) MParser.t
  val dest_cmd : (cmd_t -> 'a) -> basic_t list -> 'a
  val dest_stop : basic_t list -> unit
  val dest_skip : basic_t list -> unit
  val dest_assign : basic_t list -> Asl.Asl_term.term_t * Asl.Asl_term.term_t

  val dest_load :
    basic_t list ->
    Asl.Asl_term.term_t * Asl.Asl_term.term_t * Asl.Asl_term.term_t

  val dest_store :
    basic_t list ->
    Asl.Asl_term.term_t * Asl.Asl_term.term_t * Asl.Asl_term.term_t

  val dest_new : basic_t list -> Asl.Asl_term.term_t * Asl.Asl_term.term_t
  val dest_free : basic_t list -> Asl.Asl_term.term_t * Asl.Asl_term.term_t
  val dest_if : basic_t list -> Cond.t * t
  val dest_ifelse : basic_t list -> Cond.t * t * t
  val dest_while : basic_t list -> Cond.t * t
  val dest_branching : basic_t list -> Cond.t
  val dest_empty : 'a list -> unit
  val number : t -> t
  val cmd_terms : cmd_t -> Asl.Asl_term.Set.t
  val terms : t -> Asl.Asl_term.Set.t
  val vars : t -> Asl.Asl_term.Set.t
  val locals : Asl.Asl_term.Set.t -> t -> Asl.Asl_term.Set.t
  val cmd_modifies : ?strict:bool -> cmd_t -> Asl.Asl_term.Set.t
  val modifies : ?strict:bool -> t -> Asl.Asl_term.Set.t
  val cmd_equal : cmd_t -> cmd_t -> bool
  val equal : t -> t -> bool
  val subst_cmd : Asl.Asl_term.Asl_subst.t -> cmd_t -> cmd_t
  val subst : Asl.Asl_term.Asl_subst.t -> t -> t
  val number_width : int ref
  val indent_by : int ref
  val pp_label : ?abbr:bool -> int -> Format.formatter -> basic_t -> unit
  val pp_cmd : ?abbr:bool -> int -> Format.formatter -> basic_t -> unit
  val pp_lcmd : ?abbr:bool -> int -> Format.formatter -> basic_t -> unit
  val pp : ?abbr:bool -> int -> Format.formatter -> t -> unit
  val to_string : t -> string
end

val program_pp : Format.formatter -> Cmd.t -> unit
val pp_cmd : Format.formatter -> Cmd.t -> unit

module Seq : sig
  type t = Asl.Asl_form.t * Cmd.t

  val tagset_one : Generic.Tags.t
  val tagpairs_one : Generic.Tagpairs.t
  val tags : 'a -> Generic.Tags.t
  val tag_pairs : 'a -> Generic.Tagpairs.t
  val vars : Asl.Asl_form.t * 'a -> Asl.Asl_term.Set.t
  val terms : Asl.Asl_form.t * 'a -> Asl.Asl_term.Set.t
  val subst : Asl.Asl_subst.t -> Asl.Asl_form.t * 'a -> Asl.Asl_form.t * 'a
  val to_string : Asl.Asl_form.t * Cmd.t -> string
  val pp : Format.formatter -> Asl.Asl_form.t * Cmd.t -> unit
  val equal : Asl.Asl_form.t * Cmd.t -> Asl.Asl_form.t * Cmd.t -> bool
  val equal_upto_tags : Asl.Asl_form.t * Cmd.t -> Asl.Asl_form.t * Cmd.t -> bool
  val subsumed : Asl.Asl_form.t * Cmd.t -> Asl.Asl_form.t * Cmd.t -> bool

  val subsumed_upto_tags :
    Asl.Asl_form.t * Cmd.t -> Asl.Asl_form.t * Cmd.t -> bool
end

val program_vars : Asl.Asl_term.Set.t ref
val set_program : Cmd.t -> unit
val vars_of_program : unit -> Asl.Asl_term.Set.t
val fresh_fvar : Asl.Asl_term.Set.t -> Asl.Asl_term.term_t
val fresh_fvars : Asl.Asl_term.Set.t -> int -> Asl.Asl_term.term_t list
val fresh_evar : Asl.Asl_term.Set.t -> Asl.Asl_term.term_t
val fresh_evars : Asl.Asl_term.Set.t -> int -> Asl.Asl_term.term_t list
val parse_precondition : (Asl.Asl_form.t, 'a) MParser.t
val parse : (Asl.Asl_form.t * Cmd.t, 'a) MParser.t
val of_channel : in_channel -> Asl.Asl_form.t * Cmd.t
